;;; feather.el --- Parallel thread modern package manager        -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: tools, elisp, package
;; Version: 0.1.0
;; URL: https://github.com/conao3/feather.el
;; Package-Requires: ((emacs "26.3") (async-await "1.0"))

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

(defgroup feather nil
  "Parallel thread modern Emacs package manager."
  :group 'lisp)


;;; customize

(defcustom feather-debug-buffer "*Feather Debug*"
  "Buffer for feather debug."
  :group 'feather
  :type 'string)


;;; functions

(defun feather--warn (message &rest args)
  "Warn with `feather' type.
Display a warning message made from (format-message MESSAGE ARGS...)."
  (apply #'lwarn `(feather :warning ,message ,@args)))

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


;;; main loop

(defvar feather-install-queue-alist nil
  "Queue alist for feather.

This variable is below form.
  <alist>   := (<package> . <status>)
  <package> := symbol
  <status>  := 'queue | 'install | 'done")

(defun feather--promise-change-queue-state (pkg state)
  "Change `feather-install-queue-alist' state for PKG to STATE."
  (promise-new
   (lambda (resolve _reject)
     (setf (alist-get pkg feather-install-queue-alist) state)
     (funcall resolve (alist-get pkg feather-install-queue-alist)))))

(defun feather--promise-install-package (pkg)
  "Return promise to install PKG."
  (feather--debug 'promise-install-package
    "Install start %s" (package-desc-name pkg))
  (promise-then
   (promise:async-start
    `(lambda ()
       (let ((package-user-dir ,package-user-dir)
             (package-archives ',package-archives))
         (require 'package)
         (package-initialize)
         (package-install-from-archive ,pkg))))
   (lambda (res)
     (feather--debug 'promise-install-package
       "Install done %s" (package-desc-name pkg))
     (promise-resolve res))
   (lambda (reason)
     (promise-reject `(fail-install-package ,reason)))))

(defun feather--promise-activate-package (pkg-desc)
  "Return promise to activate PKG-DESC.
see `package-unpack'."
  (feather--debug 'promise-install-package
    "Activate start %s" (package-desc-name pkg-desc))
  (promise-new
   (lambda (resolve reject)
     (let* ((dirname (package-desc-full-name pkg-desc))
            (pkg-dir (expand-file-name dirname package-user-dir))
            (new-desc (package-load-descriptor pkg-dir)))
       (condition-case err
           (unless (equal (package-desc-full-name new-desc)
                          (package-desc-full-name pkg-desc))
             (error "The retrieved package (`%s') doesn't match what the archive offered (`%s')"
                    (package-desc-full-name new-desc) (package-desc-full-name pkg-desc)))
         ;; Activation has to be done before compilation, so that if we're
         ;; upgrading and macros have changed we load the new definitions
         ;; before compiling.
         (when (package-activate-1 new-desc :reload :deps)
           ;; FIXME: Compilation should be done as a separate, optional, step.
           ;; E.g. for multi-package installs, we should first install all packages
           ;; and then compile them.
           (package--compile new-desc)
           ;; After compilation, load again any files loaded by
           ;; `activate-1', so that we use the byte-compiled definitions.
           (package--load-files-for-activation new-desc :reload))
         (error
          (funcall reject `(fail-activate-package ,err))))
       (feather--debug 'promise-install-package
         "Activate done %s" (package-desc-name pkg-desc))
       (funcall resolve pkg-dir)))))

(async-defun feather--install-packages (pkgs)
  "Install PKGS async.
PKGS is `package-desc' list as (a b c).

This list must be processed orderd.
By because b depends a, and c depends a and b.

see `package-install' and `package-download-transaction'."
  (let ((target-pkg-name (package-desc-name
                          (car (last pkgs)))))
    (dolist (pkg pkgs)
      (let ((pkg-name (package-desc-name pkg)))
        (when (assq pkg-name feather-install-queue-alist)
          (while (not (eq 'done (alist-get pkg-name feather-install-queue-alist)))
            (feather--debug 'promise-install-packages
              (concat
               (format
                "Waiting install done for %s" pkg-name)
               (unless (eq pkg-name target-pkg-name)
                 (format " (dependency from %s)"
                         target-pkg-name))))
            (await (promise:delay 0.5))))))

    (dolist (pkg pkgs)
      (await (feather--promise-change-queue-states
              (package-desc-name pkg) 'queue))

      (dolist (pkg pkgs)
        (let ((name (package-desc-name pkg)))
          (condition-case err
              (let* ((res (await (feather--promise-change-queue-state name 'install)))
                     (res (await (feather--promise-install-package pkg)))
                     (res (await (feather--promise-activate-package pkg)))
                     (res (await (feather--promise-change-queue-state name 'done)))))
            (error
             (pcase err
               (`(error (fail-install-package ,reason))
                (feather--warn "Cannot install package.
  package: %s\n  reason: %s"
                               name reason))
               (_
                (feather--warn "Something wrong while installing package.
  package: %s\n  reason: %s"
                               name err)))))))))

  ;; ensure processed package state become 'done
  (dolist (pkg pkgs)
    (await (feather--promise-change-queue-state
            (package-desc-name pkg) 'done)))

  (package-menu--post-refresh))


;;; advice

(defvar feather-advice-alist
  '((package-install . feather--advice-package-install))
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
    (let ((name (if (package-desc-p pkg)
                    (package-desc-name pkg)
                  pkg))
          (transaction
           (if (package-desc-p pkg)
               (unless (package-installed-p pkg)
                 (package-compute-transaction (list pkg)
                                              (package-desc-reqs pkg)))
             (package-compute-transaction nil (list (list pkg))))))
      (unless (or dont-select (package--user-selected-p name))
        (package--save-selected-packages
         (cons name package-selected-packages)))
      (if (not transaction)
          (message "`%s' is already installed" name)
        (feather--debug 'package-install
          "%s depends %s"
          name (feather--resolve-dependencies name))
        (feather--debug 'package-install
          "install %s"
          (mapcar #'package-desc-name transaction))
        (feather--install-packages transaction)))))


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

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; feather.el ends here
