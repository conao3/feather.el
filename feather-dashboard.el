;;; feather-dashboard.el --- Dashboard feature for feather  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/feather.el

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

(require 'page-break-lines)

(defgroup feather-dashboard nil
  "Dashboard feature for feather."
  :group 'feather)


;;;customize

(defmacro with-feather--dashboard-buffer (&rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (debug t))
  `(let ((inhibit-read-only t))
     (with-current-buffer (or (get-buffer feather-dashboard-name)
                              (feather--dashboard-initialize))
       ,@body)))

(defcustom feather-dashboard-name "*Feather dashboard*"
  "Featehr dashboard buffer name."
  :group 'feather
  :type 'string)

(defface feather-dashboard-header
  '((((background dark))
     :background "#223377"
     :foreground "white"
     :weight bold :height 1.3 :family "Sans Serif")
    (((background light))
     :background "#abd7f0"
     :foreground "black"
     :weight bold :height 1.3 :family "Sans Serif"))
  "Face for feather-dashboard header."
  :group 'feather)

(defface feather-dashboard-state-queue
  '((((background dark))
     :foreground "#ffa500")
    (((background light))
     :goreground "#ffa500"))
  "Face for feather-dashboard state, queue.")

(defface feather-dashboard-state-wait
  '((((background dark))
     :foreground "#00bfff")
    (((background light))
     :foreground "#00bfff"))
  "Face for feather-dashboard state, wait.")

(defface feather-dashboard-state-install
  '((((background dark))
     :foreground "#ff4500")
    (((background light))
     :goreground "#ff4500"))
  "Face for feather-dashboard state, install.")

(defface feather-dashboard-state-done
  '((((background dark))
     :foreground "#7fff00")
    (((background light))
     :foreground "#7fff00"))
  "Face for feather-dashboard state, done.")

;; internal variables

(defvar feather-max-process)
(defvar feather-package-install-args)
(defvar feather-install-queue)
(defvar feather-dashboard-overlays-process nil
  "Alist of overlay for process.
Key is symbol like process1, value is overlay.")

(defvar feather-dashboard-overlays-item nil
  "Alist of overlay for item.
Key is package symbol, value is overlay.")

(declare-function feather--add-overlay "feather")
(declare-function feather--overlays-in "feather")
(declare-function feather--overlays-at "feather")
(declare-function feather--remove-all-overlays "feather")


;;; functions

(defun feather--dashboard-initialize ()
  "Initialize and return feather-dashboard buffer."
  (with-current-buffer (get-buffer-create feather-dashboard-name)
    ;; delete overlay
    (mapc #'delete-overlay
          (mapcar #'cdr (append feather-dashboard-overlays-process
                                feather-dashboard-overlays-item)))
    (setq feather-dashboard-overlays-process nil)
    (setq feather-dashboard-overlays-item nil)

    ;; initialize feather-dashboard
    (erase-buffer)
    (feather-dashboard-mode)
    (insert "*Feather dashboard*\n")
    (add-text-properties (line-beginning-position -1) (line-beginning-position)
                         '(face feather-dashboard-header))
    (dotimes (i feather-max-process)
      (let ((sym (intern (format "process%s" (1+ i)))))
        (insert (format "  %s" sym))
        (push `(,sym . ,(feather--add-overlay (point) ""))
              feather-dashboard-overlays-process)
        (newline)))
    (insert (format "\n"))
    (current-buffer)))

(defun feather--dashboard-add-new-item (sym)
  "Add package SYM to feather-dashboard item section."
  (with-feather--dashboard-buffer
    (goto-char (point-min))
    (forward-page)
    (forward-line)
    (beginning-of-line)
    (insert (format "  %s" sym))
    (push `(,sym . ,(feather--add-overlay (point) ""))
          feather-dashboard-overlays-item)
    (newline)))

(defun feather--dashboard-change-process-state (sym state &optional info)
  "Change state of process SYM for PKG to STATE with additional INFO.
INFO is optional alist.
- (queue)
  - (none)
- wait
  - TARGET-PKG is install requested package.
  - PKG is package waiting to be installed as symbol.
- install
  - TARGET-PKG is install requested package.
  - PKG is package installing as symbol.
- done
  - (none)"
  (let* ((target-pkg (alist-get 'target-pkg info))
         (pkg (alist-get 'pkg info))
         (alist (gethash target-pkg feather-install-queue)))
    (when-let ((ov (alist-get sym feather-dashboard-overlays-process)))
      (overlay-put ov
                   'after-string
                   (format " -- %s"
                           (cond
                            ((eq state 'queue)
                             (propertize "queue"
                                         'face 'feather-dashboard-state-queue))
                            ((eq state 'wait)
                             (mapconcat
                              'prin1-to-string
                              (alist-get 'depends alist)
                              " "))
                            ((eq state 'install)
                             (mapconcat
                              'prin1-to-string
                              (alist-get 'depends alist)
                              " "))
                            ((eq state 'done)
                             (propertize "done"
                                         'face 'feather-dashboard-state-done))))))))

(defun feather--dashboard-change-item-state (sym state &optional info)
  "Change state of package SYM to STATE with additional INFO.
INFO is optional alist.
- queue
  - (none)
- wait
  - DEP-PKG is a dependency for package SYM waiting to be installed as symbol.
- install
  - (none)
- done
  - VERSION is package version as string."
  (when-let ((ov (alist-get sym feather-dashboard-overlays-item)))
    (overlay-put ov
                 'after-string
                 (format " %s"
                         (cond
                          ((eq state 'queue)
                           (propertize "queue"
                                       'face 'feather-dashboard-state-queue))
                          ((eq state 'wait)
                           (concat
                            (propertize "waiting"
                                        'face 'feather-dashboard-state-wait)
                            (when-let (dep-pkg (alist-get 'dep-pkg info))
                              (format " %s to be installed"
                                      dep-pkg))))
                          ((eq state 'install)
                           (propertize "install"
                                       'face 'feather-dashboard-state-install))
                          ((eq state 'done)
                           (propertize "done"
                                       'face 'feather-dashboard-state-done)))))))

(define-derived-mode feather-dashboard-mode special-mode "FeatherDashboard"
  "Major mode for feather dashboard."
  (add-to-list 'page-break-lines-modes 'feather-dashboard-mode)
  (page-break-lines-mode +1))

(provide 'feather-dashboard)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; feather-dashboard.el ends here
