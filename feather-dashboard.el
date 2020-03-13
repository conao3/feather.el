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

;; Dashboard feature for feather.


;;; Code:

(require 'page-break-lines)
(require 'subr-x)

(defgroup feather-dashboard nil
  "Dashboard feature for feather."
  :group 'feather)


;; customize

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

(defface feather-dashboard-state-error
  '((((background dark))
     :foreground "#b22222")
    (((background light))
     :foreground "#b22222"))
  "Face for feather-dashboard state, error.")

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


;;; overlay

(defun feather-dashboard--add-overlay (pos str)
  "Add overlay to display STR at POS symbol."
  (let* ((bound (save-excursion
                  (goto-char pos)
                  (bounds-of-thing-at-point 'sexp)))
         (beg (car bound))
         (end (cdr bound))
         (ov (make-overlay beg end)))
    (overlay-put ov 'feather-dashboard-overlay t)
    (overlay-put ov 'after-string str)
    ov))

(defun feather-dashboard--overlays-in (beg end)
  "Get all feather-dashboard overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'feather-dashboard-overlay))
   (overlays-in beg end)))

(defun feather-dashboard--remove-all-overlays ()
  "Remove all `feather' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay (feather-dashboard--overlays-in (point-min) (point-max)))))


;;; functions

(declare-function feather--change-running-state "feather")
(declare-function feather--get-feather-running "feather")
(declare-function feather--push-package-install-args "feather")
(declare-function feather--pop-package-install-args "feather")
(declare-function feather--get-package-install-args "feather")
(declare-function feather--add-install-queue "feather")
(declare-function feather--get-install-queue "feather")
(declare-function feather--change-install-queue-status "feather")
(declare-function feather--get-install-queue-status "feather")

(defmacro with-feather-dashboard-buffer (&rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (debug t))
  `(let ((inhibit-read-only t))
     (with-current-buffer (or (get-buffer feather-dashboard-name)
                              (feather-dashboard--initialize))
       (unless (= feather-max-process
                  (length feather-dashboard-overlays-process))
         (feather-dashboard--initialize))
       ,@body)))

(defun feather-dashboard--initialize ()
  "Initialize and return feather-dashboard buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create feather-dashboard-name)
      ;; delete overlay
      (feather-dashboard--remove-all-overlays)
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
          (push `(,sym . ,(feather-dashboard--add-overlay (point) ""))
                feather-dashboard-overlays-process)
          (newline)))
      (insert (format "\n"))
      (current-buffer))))

(defun feather-dashboard--pop-dashboard (_info)
  "Pop dashboard buffer.
This function is invoked as hook function with INFO argument.
see `feather--push-package-install-args.'"
  (pop-to-buffer (with-feather-dashboard-buffer (current-buffer))))

(defun feather-dashboard--add-new-item (info)
  "Add package to feather-dashboard item section.
This function is invoked as hook function with INFO argument.
see `feather--push-package-install-args.'"
  (let-alist info
    (seq-let (pkg _dont-select) .val
      (let ((pkg-name (if (package-desc-p pkg)
                          (package-desc-name pkg)
                        pkg)))
        (with-feather-dashboard-buffer
         (goto-char (point-min))
         (forward-page)
         (forward-line)
         (beginning-of-line)
         (insert (format "  %s" pkg-name))
         (push `(,pkg-name . ,(feather-dashboard--add-overlay (point) ""))
               feather-dashboard-overlays-item)
         (newline))))))

(defun feather-dashboard--create-process-status-str (key val depends queue installed)
  "Create feather-dashboard process status string.
Using KEY, VAL, DEPENDS, QUEUE, INSTALLED."
  (mapconcat (lambda (elm)
               (seq-let (pkg _ver) elm
                 (propertize (prin1-to-string elm) 'face
                             (cond
                              ((eq pkg key)
                               (cl-case val
                                 (queue   'feather-dashboard-state-queue)
                                 (wait    'feather-dashboard-state-wait)
                                 (install 'feather-dashboard-state-install)
                                 (done    'feather-dashboard-state-done)))
                              ((memq pkg installed)
                               'feather-dashboard-state-done)
                              ((memq pkg queue)
                               'feather-dashboard-state-queue)
                              (t
                               'feather-dashboard-state-done)))))
             depends " "))

(defun feather-dashboard--change-process-status (info)
  "Change status of process.
This functino is invoked as hook function with INFO argument.
see `feather--change-install-queue-status'"
  (let-alist info
    (let ((key .key)
          (val .val))
      (let-alist (feather--get-install-queue key)
        (let-alist (feather--get-install-queue .targetpkg)
          (when-let ((ov (alist-get (intern (format "process%s" .process))
                                    feather-dashboard-overlays-process)))
            (overlay-put ov
                         'after-string
                         (format " -- %s"
                                 (cl-case val
                                   (queue
                                    (propertize "queue"
                                                'face 'feather-dashboard-state-queue))
                                   (wait
                                    (feather-dashboard--create-process-status-str
                                     key val .depends .queue .installed))
                                   (install
                                    (feather-dashboard--create-process-status-str
                                     key val .depends .queue .installed))
                                   (error
                                    (propertize "done"
                                                'face 'feather-dashboard-state-done))
                                   (done
                                    (propertize "done"
                                                'face 'feather-dashboard-state-done)))))))))))

(defun feather-dashboard--change-item-status (info)
  "Change state of package in feather-dashboard item section.
This function is invoked as hook function with INFO argument.
see `feather--change-install-queue-status'."
  (let-alist info
    (when-let ((ov (alist-get .key feather-dashboard-overlays-item)))
      (overlay-put ov
                   'after-string
                   (format " %s"
                           (cl-case .val
                             (queue
                              (propertize "queue"
                                          'face 'feather-dashboard-state-queue))
                             (wait
                              (propertize
                               (concat
                                "waiting"
                                (when .dep-pkg
                                  (format " %s to be installed" .dep-pkg)))
                               'face 'feather-dashboard-state-wait))
                             (install
                              (propertize "install"
                                          'face 'feather-dashboard-state-install))
                             (error
                              (propertize
                               (concat
                                "error"
                                (cl-case .err-type
                                  (unknown
                                   (format " unknown: %s" .err-reason))))
                               'face 'feather-dashboard-state-error))
                             (done
                              (propertize "done"
                                          'face 'feather-dashboard-state-done))))))))


;;; main

(defvar feather-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" nil)
    map))

(define-derived-mode feather-dashboard-mode special-mode "FeatherDashboard"
  "Major mode for feather dashboard."
  (add-to-list 'page-break-lines-modes 'feather-dashboard-mode)
  (page-break-lines-mode +1))

(provide 'feather-dashboard)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; feather-dashboard.el ends here
