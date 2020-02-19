;;; feather.el --- Parallel thread modern package manager        -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience package
;; Version: 0.1.0
;; URL: https://github.com/conao3/feather.el
;; Package-Requires: ((emacs "26.3") (async-await "1.0") (ppp "1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Parallel thread modern Emacs package manager.


;;; Code:

(require 'package)
(require 'async-await)
(require 'ppp)

(defgroup feather nil
  "Parallel thread modern Emacs package manager."
  :group 'applications)


;;; customize

(defcustom feather-pallarel-process-number 8
  "Count of pallarel process number."
  :group 'feather
  :type 'number)


;;; functions

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

(defvar feather-running nil
  "If non-nil, running feather main process.")

(defvar feather-package-install-args nil
  "List of `package-install' args.
see `feather--advice-package-install' and `feather--main-process'.")

(defvar feather-install-queue (make-hash-table :test 'eq)
  "All install queues, including dependencies.

Key is package name as symbol.
Value is alist.
  - NAME is package as string.
  - STATUS is install status one of (queue install done).
  - PKG is `package-desc'.")

(defvar feather-current-pallarel-process-number 0
  "Count of current processed parallel Emacs.")

(defun feather--promise-change-queue-state (pkg state)
  "Change `feather-install-queue' state for PKG to STATE."
  (promise-new
   (lambda (resolve _reject)
     (setf (alist-get pkg feather-install-queue) state)
     (funcall resolve (alist-get pkg feather-install-queue)))))

(defun feather--promise-install-package (pkg)
  "Return promise to install PKG."
  (ppp-debug 'feather
    "Start install\n%s"
    (ppp-plist-to-string
     (list :package (package-desc-name pkg))))
  (promise-then
   (promise:async-start
    `(lambda ()
       (let ((package-user-dir ,package-user-dir)
             (package-archives ',package-archives))
         (require 'package)
         (package-initialize)
         (package-install-from-archive ,pkg))))
   (lambda (res)
     (ppp-debug 'feather
       "Done install\n%s"
       (ppp-plist-to-string
        (list :package (package-desc-name pkg))))
     (promise-resolve res))
   (lambda (reason)
     (promise-reject `(fail-install-package ,reason)))))

(defun feather--promise-activate-package (pkg-desc)
  "Return promise to activate PKG-DESC.
see `package-unpack'."
  (ppp-debug 'feather
    "Start activate\n%s"
    (ppp-plist-to-string
     (list :package (package-desc-name pkg-desc))))
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
       (ppp-debug 'feather
         "Done activate\n%s"
         (ppp-plist-to-string
          (list :package (package-desc-name pkg-desc))))
       (funcall resolve pkg-dir)))))

(async-defun feather--install-package (pkg)
  "Install PKG async.
PKG is `package-desc'."
  (let ((name (package-desc-name pkg)))
    (condition-case err
        (let* ((res (await (feather--promise-change-queue-state name 'install)))
               (res (await (feather--promise-install-package pkg)))
               (res (await (feather--promise-activate-package pkg)))
               (res (await (feather--promise-change-queue-state name 'done)))))
      (error
       (pcase err
         (`(error (fail-install-package ,reason))
          (ppp-debug :level :warning 'feather
            "Cannot install package\n%s"
            (ppp-plist-to-string
             (list :package name
                   :reason reason))))
         (_
          (ppp-debug :level :warning 'feather
            "Something wrong while installing package\n%s"
            (ppp-plist-to-string
             (list :package name
                   :reason err)))))))))

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
        (when (assq pkg-name feather-install-queue)
          (while (not (eq 'done (alist-get pkg-name feather-install-queue)))
            (ppp-debug 'feather
              "Waiting install done\n%s"
              (ppp-plist-to-string
               (list :package pkg-name
                     :dependency-from target-pkg-name)))
            (await (promise:delay 0.5))))))

    (while (< feather-pallarel-process-number
              feather-current-pallarel-process-number)
      (ppp-debug 'feather
        "A parallel limit has been reached\n%s"
        (ppp-plist-to-string
         (list :package target-pkg-name)))
      (await (promise:delay 0.5))))

  ;; increment current-pallarel-process-number
  (cl-incf feather-current-pallarel-process-number)

  (dolist (pkg pkgs)
    (await (feather--promise-change-queue-state
            (package-desc-name pkg) 'queue)))

  ;; `package-download-transaction'
  (dolist (pkg pkgs)
    (await (feather--install-package pkg)))

  ;; decrement current-pallarel-process-number
  (cl-decf feather-current-pallarel-process-number)

  ;; ensure processed package state become 'done
  (dolist (pkg pkgs)
    (await (feather--promise-change-queue-state
            (package-desc-name pkg) 'done)))

  (package-menu--post-refresh))

(async-defun feather--main-process ()
  "Main process for feather."
  (setq feather-running t)

  (while feather-package-install-args
    (seq-let (pkg dont-select) (pop feather-package-install-args)
      (let ((name (if (package-desc-p pkg) (package-desc-name pkg) pkg))
            (transaction
             (if (package-desc-p pkg)
                 (unless (package-installed-p pkg)
                   (package-compute-transaction (list pkg)
                                                (package-desc-reqs pkg)))
               (package-compute-transaction nil (list (list pkg))))))
        (ppp-debug 'feather
          (prin1-to-string feather-package-install-args))
        (unless (or dont-select (package--user-selected-p name))
          (package--save-selected-packages
           (cons name package-selected-packages)))
        (if (not transaction)
            (message "`%s' is already installed" name)
          (ppp-debug :break t 'feather
            "Install package\n%s"
            (ppp-plist-to-string
             (list :target name
                   :depends (feather--resolve-dependencies name)
                   :queued (mapcar #'package-desc-name transaction))))
          (feather--install-packages transaction)))))

  (add-hook 'post-command-hook #'package-menu--post-refresh)
  (setq feather-running nil))


;;; advice

(defvar feather-advice-alist
  '((package-install . feather--advice-package-install))
  "Alist for feather advice.
See `feather--setup' and `feather--teardown'.")

(defun feather--advice-package-install (_fn &rest args)
  "Around advice for FN with ARGS.
This code based package.el bundled Emacs-26.3.
See `package-install'."
  (push args feather-package-install-args)
  (unless feather-running
    (feather--main-process)))


;;; main

(defun feather--setup ()
  "Setup feather."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) feather-advice-alist)
    (advice-add sym :around fn)))

(defun feather--teardown ()
  "Teardown feather."
  (interactive)
  (pcase-dolist (`(,sym . ,fn) feather-advice-alist)
    (advice-remove sym fn)))

;;;###autoload
(define-minor-mode feather-mode
  "Toggle feather."
  :global t
  (if feather-mode
      (feather--setup)
    (feather--teardown)))

(provide 'feather)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; feather.el ends here
