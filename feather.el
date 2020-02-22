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

(defcustom feather-max-process 8
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
  - STATUS is install status one of (queue install done).")

(defun feather--promise-install-package (pkg-desc)
  "Return promise to install PKG-DESC."
  (ppp-debug 'feather
    (ppp-plist-to-string
     (list :status 'start-install
           :package (package-desc-name pkg-desc))))
  (promise-then
   (promise:async-start
    `(lambda ()
       (let ((package-user-dir ,package-user-dir)
             (package-archives ',package-archives))
         (require 'package)
         (package-initialize)
         (package-install-from-archive ,pkg-desc))))
   (lambda (res)
     (ppp-debug 'feather
       (ppp-plist-to-string
        (list :status 'done-install
              :package (package-desc-name pkg-desc))))
     (promise-resolve res))
   (lambda (reason)
     (promise-reject `(fail-install-package ,reason)))))

(defun feather--promise-activate-package (pkg-desc)
  "Return promise to activate PKG-DESC.
see `package-unpack'."
  (ppp-debug 'feather
    (ppp-plist-to-string
     (list :status 'start-activate
           :package (package-desc-name pkg-desc))))
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
         (ppp-plist-to-string
          (list :status 'done-activate
                :package (package-desc-name pkg-desc))))
       (funcall resolve pkg-dir)))))

(async-defun feather--install-packages (pkg-descs)
  "Install PKGS async.
PKGS is `package-desc' list as (a b c).

This list must be processed orderd; b depends (a), and c depends (a b).

see `package-install' and `package-download-transaction'."
  (let ((target-pkg-name (package-desc-name (car (last pkg-descs)))))
    (dolist (pkgdesc pkg-descs)
      (let ((pkg-name (package-desc-name pkgdesc)))
        (when-let (alist (gethash pkg-name feather-install-queue))
          (while (not (eq 'done (alist-get 'status alist)))
            (ppp-debug 'feather
              "Wait for dependencies to be installed\n%s"
              (ppp-plist-to-string
               (list :package pkg-name
                     :dependency-from target-pkg-name)))
            (await (promise:delay 0.5)))))))

  ;; set the status of the package to be installed to queue
  (dolist (pkgdesc pkg-descs)
    (let ((pkg-name (package-desc-name pkgdesc)))
      (if (gethash pkg-name feather-install-queue)
          (setf (alist-get 'status (gethash pkg-name feather-install-queue)) 'queue)
        (puthash pkg-name '((status . queue)) feather-install-queue))))

  ;; `package-download-transaction'
  (dolist (pkgdesc pkg-descs)
    (let ((pkg-name (package-desc-name pkgdesc)))
      (setf (alist-get 'status (gethash pkg-name feather-install-queue)) 'install)
      (condition-case err
          (progn
            (await (feather--promise-install-package pkgdesc))
            (await (feather--promise-activate-package pkgdesc)))
        (error
         (pcase err
           (`(error (fail-install-package ,reason))
            (ppp-debug :level :warning 'feather
              "Cannot install package\n%s"
              (ppp-plist-to-string
               (list :package pkg-name
                     :reason reason))))
           (_
            (ppp-debug :level :warning 'feather
              "Something wrong while installing package\n%s"
              (ppp-plist-to-string
               (list :package pkg-name
                     :reason err)))))))
      (setf (alist-get 'status (gethash pkg-name feather-install-queue)) 'done))))

(async-defun feather--main-process ()
  "Main process for feather."

  ;; preprocess
  (setq feather-running t)
  (await (promise:delay 1))             ; wait for continuous execution

  ;; feather-package-install-args may increase during execution of this loop
  (while feather-package-install-args
    (await
     (promise-concurrent-no-reject-immidiately
         feather-max-process (length feather-package-install-args)
       (lambda (index)
         (seq-let (pkg dont-select) (pop feather-package-install-args)

           ;; `package-install'

           ;; moved last of this function
           ;; (add-hook 'post-command-hook #'package-menu--post-refresh)
           (let ((name (if (package-desc-p pkg)
                           (package-desc-name pkg)
                         pkg)))
             (unless (or dont-select (package--user-selected-p name))
               (package--save-selected-packages
                (cons name package-selected-packages)))
             (if-let* ((transaction
                        (if (package-desc-p pkg)
                            (unless (package-installed-p pkg)
                              (package-compute-transaction (list pkg)
                                                           (package-desc-reqs pkg)))
                          (package-compute-transaction nil (list (list pkg))))))
                 (progn
                   (ppp-debug :break t 'feather
                     (ppp-plist-to-string
                      (list :index (1+ index)
                            :status 'install
                            :target name
                            :depends (feather--resolve-dependencies name)
                            :queued (mapcar #'package-desc-name transaction))))
                   (feather--install-packages transaction))
               (message "`%s' is already installed" name))))))))

  ;; postprocess
  (package-menu--post-refresh)
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
  (pcase-dolist (`(,sym . ,fn) feather-advice-alist)
    (advice-add sym :around fn)))

(defun feather--teardown ()
  "Teardown feather."
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
