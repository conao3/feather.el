;;; feather.el --- Parallel thread modern package manager        -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: tools, elisp, package
;; Version: 0.1.0
;; URL: https://github.com/conao3/feather.el
;; Package-Requires: ((emacs "26.1") (async-await "1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Affero GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public License
;; along with this program.  If not, see  <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parallel thread modern Emacs package manager.


;;; Code:

(require 'package)
(require 'async-await)
(require 'feather-polyfill)

(defgroup feather nil
  "Parallel thread modern Emacs package manager."
  :group 'lisp)


;;; customize

(defcustom feather-debug-buffer "*Feather Debug*"
  "Buffer for feather debug."
  :group 'feather
  :type 'string)


;;; functions

(defun feather--debug (&rest args)
  "Output debug information.
FORMAT and FORMAT-ARGS passed `format'.
If BREAK is non-nil, output page break before output string.

ARGS accept (fn &rest FORMAT-ARGS &key buffer break).

\(fn FN FORMAT &rest FORMAT-ARGS &key buffer break)"
  (declare (indent defun))
  (let (fn format format-args buf break elm)
    (while (keywordp (setq elm (pop args)))
      (cond ((eq :buffer elm)
             (setq buf (pop args)))
            ((eq :break elm)
             (setq break (pop args)))
            (t
             (error "Unknown keyword: %s" elm))))
    (setq fn elm)
    (setq format (pop args))
    (setq format-args args)
    (let ((buf* (or buf (get-buffer-create feather-debug-buffer))))
      (with-current-buffer buf*
        (emacs-lisp-mode)
        (display-buffer buf*)
        (let ((condition (equal (point) (point-max))))
          (save-excursion
            (goto-char (point-max))
            (when break
              (insert "\n"))
            (insert
             (format "%s: %s\n" fn (apply #'format `(,format ,@format-args)))))
          (when condition
            (goto-char (point-max))
            (set-window-point (get-buffer-window buf*) (point-max))))))))

(defun feather--resolve-dependencies-1 (pkgs)
  "Resolve dependencies for PKGS using package.el cache.
PKGS accepts package name symbol or list of these.
Return a list of dependencies, allowing duplicates."
  (when pkgs
    (mapcan
     (lambda (pkg)
       (let* ((pkg* (if (symbolp pkg) (list pkg '(0 1)) pkg))
              (elm  (assq (car pkg*) package-archive-contents))
              (req  (and elm (package-desc-reqs (cadr elm)))))
         (append req (funcall #'feather--resolve-dependencies-1 req))))
     (if (symbolp pkgs) (list pkgs) pkgs))))

(defun feather--resolve-dependencies (pkg)
  "Resolve dependencies for PKG.
PKGS accepts package name symbol.
Return a list of dependencies, duplicates are resolved by more
restrictive."
  (let (ret)
    (dolist (req (funcall #'feather--resolve-dependencies-1 pkg))
      (let ((sym (car  req))
            (ver (cadr req)))
        (if (assq sym ret)
            (when (version-list-< (car (alist-get sym ret)) ver)
              (setf (alist-get sym ret) (list ver)))
          (push req ret))))
    (append
     `((,pkg ,(package-desc-version
               (cadr (assq 'helm package-archive-contents)))))
     (nreverse ret))))


;;; advice
(defvar feather-advice-alist
  '((package-install              . feather--advice-package-install)
    (package-download-transaction . feather--advice-package-download-transaction)
    (package-install-from-archive . feather--advice-package-install-from-archive)
    (package-unpack               . feather--advice-package-unpack)
    (package-untar-buffer         . feather--advice-package-untar-buffer)
    (package--make-autoloads-and-stuff . feather--advice-package--make-autoloads-and-stuff)
    (package-generate-autoloads        . feather--advice-package-generate-autoloads)
    (package-generate-description-file . feather--advice-package-generate-description-file)
    (package-activate                  . feather--advice-package-activate)
    (package-activate-1                . feather--advice-package-activate-1)
    (package--compile                  . feather--advice-package--compile)
    (package--load-files-for-activation . feather--advice-package--load-files-for-activation))
  "Alist for feather advice.
See `feather-setup' and `feather-teardown'.")

(defun feather--advice-package-install (_fn &rest args)
  "Around advice for FN with ARGS.
This code based package.el bundled Emacs-26.3.
See `package-install'."
  (seq-let (pkg dont-select) args
    (let ((name (if (package-desc-p pkg)
                    (package-desc-name pkg)
                  pkg)))
      (feather--debug :break t
        'package-install "%s" name))

    ;; `package-install'
    (add-hook 'post-command-hook #'package-menu--post-refresh)
    (let ((name (if (package-desc-p pkg)
                    (package-desc-name pkg)
                  pkg))
          (transaction
           (if (package-desc-p pkg)
               (unless (package-installed-p pkg)
                 (package-compute-transaction (list pkg)
                                              (package-desc-reqs pkg)))
             (package-compute-transaction () (list (list pkg))))))
      (unless (or dont-select (package--user-selected-p name))
        (package--save-selected-packages
         (cons name package-selected-packages)))
      (if (not transaction)
          (message "`%s' is already installed" name)
        (feather--debug 'package-install
          "%s depends %s"
          name (feather--resolve-dependencies name))

        ;; `package-download-transaction'
        (mapc #'package-install-from-archive transaction)))))

(defun feather--advice-package-download-transaction (fn &rest args)
  "Around advice for FN with ARGS.
See `package-download-transaction'."
  (seq-let (packages) args
    (feather--debug 'package-download-transaction
      "%s"
      (mapcar (lambda (elm) (package-desc-name elm)) packages))
    (apply fn args)))

(defun feather--advice-package-install-from-archive (fn &rest args)
  "Around advice for FN with ARGS.
See `package-install-from-archive'."
  (seq-let (pkg-desc) args
    (let ((name (package-desc-name pkg-desc))
          (location (package-archive-base pkg-desc))
          (file (concat (package-desc-full-name pkg-desc)
                        (package-desc-suffix pkg-desc))))
      (feather--debug 'package-install-from-archive
        "%s" name)
      (feather--debug 'package-install-from-archive
        "fetch %s" (concat location file)))
    (apply fn args)))

(defun feather--advice-package-unpack (fn &rest args)
  "Around advice for FN with ARGS.
See `package-unpack'."
  (seq-let (pkg-desc) args
    (let* ((name (package-desc-name pkg-desc))
           (dirname (package-desc-full-name pkg-desc))
           (kind (package-desc-kind pkg-desc)))
      (feather--debug 'package-unpack "%s" name)
      (feather--debug 'package-unpack
        "unpack %s, kind %s" dirname kind))
    (apply fn args)))

(defun feather--advice-package-untar-buffer (fn &rest args)
  "Around advice for FN with ARGS.
See `package-untar-buffer'."
  (seq-let (dir) args
    (feather--debug 'package-untar-buffer "%s" dir)
    (apply fn args)))

(defun feather--advice-package--make-autoloads-and-stuff (fn &rest args)
  "Around advice for FN with ARGS.
See `package--make-autoloads-and-stuff'."
  (seq-let (pkg-desc pkg-dir) args
    (let ((name (package-desc-name pkg-desc)))
      (feather--debug 'package--make-autoloads-and-stuff
        "%s in %s" name pkg-dir))
    (apply fn args)))

(defun feather--advice-package-generate-autoloads (fn &rest args)
  "Around advice for FN with ARGS.
See `package-generate-autoloads'."
  (seq-let (name pkg-dir) args
    (feather--debug 'package-generate-autoloads
      "%s saved in %s" name pkg-dir)
    (apply fn args)))

(defun feather--advice-package-generate-description-file (fn &rest args)
  "Around advice for FN with ARGS.
See `package-generate-description-file'."
  (seq-let (pkg-desc pkg-file) args
    (let ((name (package-desc-name pkg-desc)))
      (feather--debug 'package-generate-description-file
        "%s %s" name pkg-file))
    (apply fn args)))

(defun feather--advice-package-activate (fn &rest args)
  "Around advice for FN with ARGS.
See `package-activate'."
  (seq-let (package force) args
    (feather--debug 'package-activate
      "%s, force activate: %s" package force)
    (apply fn args)))

(defun feather--advice-package-activate-1 (fn &rest args)
  "Around advice for FN with ARGS.
See `package-activate-1'."
  (seq-let (pkg-desc reload deps) args
    (let ((name (package-desc-name pkg-desc)))
      (feather--debug 'package-activate-1
        "%s, reload: %s, deps: %s" name reload deps))
    (apply fn args)))

(defun feather--advice-package--compile (_fn &rest args)
  "Around advice for FN with ARGS.
This function is based `package' bundled Emacs-26.3.
See `package--compile'."
  (seq-let (pkg-desc) args
    (let ((name (package-desc-name pkg-desc)))
      (feather--debug 'package--compile
        "%s" name))
    (promise-chain
        (promise:async-start
         `(lambda ()
            (let ((warning-minimum-level :error)
                  (load-path ',load-path))
              (byte-recompile-directory ,(package-desc-dir pkg-desc) 0 t))))
      (then (lambda (res)
              (feather--debug 'package--compile
                "done. res:%s" (prin1-to-string res)))
            (lambda (res)
              (feather--debug 'package--compile
                "fail! res:%s" (prin1-to-string res))))
      (then (lambda (_res)
              (seq-let (pkg-desc reload) `(,pkg-desc :reload)
                (let* ((loaded-files-list (when reload
                                            (package--list-loaded-files (package-desc-dir pkg-desc)))))
                  ;; Add to load path, add autoloads, and activate the package.
                  (package--activate-autoloads-and-load-path pkg-desc)
                  ;; Call `load' on all files in `package-desc-dir' already present in
                  ;; `load-history'.  This is done so that macros in these files are updated
                  ;; to their new definitions.  If another package is being installed which
                  ;; depends on this new definition, not doing this update would cause
                  ;; compilation errors and break the installation.
                  (with-demoted-errors "Error in package--load-files-for-activation: %s"
                    (mapc (lambda (feature) (load feature nil t))
                          ;; Skip autoloads file since we already evaluated it above.
                          (remove (file-truename (package--autoloads-file-name pkg-desc))
                                  loaded-files-list)))))))
      (then (lambda (res)
              (feather--debug 'package--compile
                "done. res:%s" (prin1-to-string res)))
            (lambda (res)
              (feather--debug 'package--compile
                "fail! res:%s" (prin1-to-string res)))))))

(defun feather--advice-package--load-files-for-activation (_fn &rest args)
  "Around advice for FN with ARGS.
See `package--load-files-for-activation'."
  (seq-let (pkg-desc reload) args
    (let ((name (package-desc-name pkg-desc)))
      (feather--debug 'package--load-files-for-activation
        "%s, reload: %s" name reload))
    ;; (apply fn args)
    ))


;;; main

;;;###autoload
(defun feather-setup ()
  "Setup feather."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) feather-advice-alist)
    (advice-add sym :around fn)))

;;;###autoload
(defun feather-teardown ()
  "Setup feather."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) feather-advice-alist)
    (advice-remove sym fn)))

(provide 'feather)
;;; feather.el ends here
