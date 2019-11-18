;;; feather.el --- Parallel thread modern Emacs package manager        -*- lexical-binding: t; -*-

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

(defun feather--debug (fn &rest args)
  "Output debug information for FN in BUF.
FORMAT and FORMAT-ARGS passed `format'.
If BREAK is non-nil, output page break before output string.

\(fn FN FORMAT &rest FORMAT-ARGS &key buffer break)"
  (declare (indent 1))
  (let (format format-args buf break)
    (while (keywordp (setq elm (pop args)))
      (cond ((eq :buffer elm)
             (setq buf (pop args)))
            ((eq :break elm)
             (setq break (pop args)))
            (_
             (error "%s is unknown keyword." elm))))
    (setq format elm)
    (setq format-args args)
    `(,fn ,format ,format-args ,buf ,break)))

(defun feather--debug (fn format format-args &optional buf break)
  "Output debug information for FN in BUF.
FORMAT and FORMAT-ARGS passed `format'.
If BREAK is non-nil, output page break before output string."
  (declare (indent 1))
  (let ((buf* (or buf (get-buffer-create feather-debug-buffer))))
    (with-current-buffer buf*
      (display-buffer buf*)
      (goto-char (point-max))
      (when break
        (insert "\n"))
      (insert
       (format "%s: %s\n" fn (apply #'format `(,format ,@format-args)))))))

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
  '((package-install . feather--advice-package-install)
    (package-compute-transaction . feather--advice-package-compute-transaction)
    (package-download-transaction . feather--advice-package-download-transaction)
    (package-install-from-archive . feather--advice-package-install-from-archive)
    (package-unpack . feather--advice-package-unpack)
    (package-untar-buffer . feather--advice-package-untar-buffer))
  "Alist for feather advice.
See `feather-setup' and `feather-teardown'.")

(defun feather--advice-package-install (fn &rest args)
  "Around advice for FN with ARGS.
See `package-install'."
  (seq-let (pkg _dont-select) args
    (let ((name (if (package-desc-p pkg)
                    (package-desc-name pkg)
                  pkg)))
      (feather--debug 'package-install "%s" (list name) nil 'break)
      (feather--debug 'package-install
        "%s depends %s"
        (list name
              (feather--resolve-dependencies name))))
    (apply fn args)))

(defun feather--advice-package-compute-transaction (fn &rest args)
  "Around advice for FN with ARGS.
See `package-compute-transaction'."
  (seq-let (packages requirements _seen) args
    (feather--debug 'package-compute-transaction
      "%s, required %s"
      (list (mapcar
             (lambda (elm)
               (package-desc-name elm))
             packages)
            (prin1-to-string requirements)))
    (apply fn args)))

(defun feather--advice-package-download-transaction (fn &rest args)
  "Around advice for FN with ARGS.
See `package-download-transaction'."
  (seq-let (packages) args
    (feather--debug 'package-download-transaction
      "%s"
      (list (mapcar
             (lambda (elm)
               (package-desc-name elm))
             packages)))
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
        "%s" (list name))
      (feather--debug 'package-install-from-archive
        "fetch %s" (list (concat location file))))
    (apply fn args)))

(defun feather--advice-package-unpack (fn &rest args)
  "Around advice for FN with ARGS.
See `package-unpack'."
  (seq-let (pkg-desc) args
    (let* ((name (package-desc-name pkg-desc))
           (dirname (package-desc-full-name pkg-desc))
           (kind (package-desc-kind pkg-desc)))
      (feather--debug 'package-unpack "%s" (list name))
      (feather--debug 'package-unpack
        "unpack %s, kind %s" (list dirname kind)))
    (apply fn args)))

(defun feather--advice-package-untar-buffer (fn &rest args)
  "Around advice for FN with ARGS.
See `package-untar-buffer'."
  (pcase-let ((`(,dir) args))
    (feather--debug 'package-untar-buffer "%s" (list dir))
    (apply fn args)))


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
