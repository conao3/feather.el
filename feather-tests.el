;;; feather-tests.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;


;;; Code:

(load "cort-test")
(require 'feather)

(defun feather--debug (fn format format-args &optional buf)
    (declare (indent 1))
    (let ((buf* (or buf (get-buffer-create "*Feather Debug*"))))
      (with-current-buffer buf*
        (display-buffer buf*)
        (goto-char (point-max))
        (when (eq fn 'package-install)
          (insert "\n"))
        (insert
         (format "%s: %s\n" fn (apply #'format `(,format ,@format-args)))))))

(define-advice package-install
    (:around (fn &rest args) feather)
  (pcase-let ((`(,pkg ,dont-select) args))
    (let ((req-fn)
          (pkg-name (if (package-desc-p pkg) (package-desc-name pkg) pkg)))
      (setq req-fn (lambda (pkgs)
                 (when pkgs
                   (mapcan
                    (lambda (pkg)
                      (let* ((pkg* (if (symbolp pkg) (list pkg '(0 1)) pkg))
                             (elm  (assq (car pkg*) package-archive-contents))
                             (req  (and elm (package-desc-reqs (cadr elm)))))
                        (append req (funcall req-fn req))))
                    (if (symbolp pkgs) (list pkgs) pkgs)))))
      (feather--debug 'package-install "%s" (list pkg-name))
      (apply fn args)
      (feather--debug 'debug--package-install "%s" (list (prin1-to-string (funcall req-fn pkg-name)))))))

(define-advice package-compute-transaction
    (:around (fn &rest args) feather)
  (pcase-let ((`(,packages ,requirements ,seen) args))
    (feather--debug 'package-compute-transaction
      "%s, required %s"
      (list (mapcar
             (lambda (elm)
               (package-desc-name elm))
             packages)
            (prin1-to-string requirements)))
    (apply fn args)))

(define-advice package-download-transaction
    (:around (fn &rest args) feather)
  (pcase-let ((`(,packages) args))
    (feather--debug 'package-download-transaction
      "%s"
      (list (mapcar
             (lambda (elm)
               (package-desc-name elm))
             packages)))
    (apply fn args)))

(define-advice package-install-from-archive
    (:around (fn &rest args) feather)
  (pcase-let ((`(,pkg-desc) args))
    (feather--debug 'package-install-from-archive
      "%s"
      (list (package-desc-name pkg-desc)))
    (apply fn args)))

;; (provide 'feather-tests)
;;; feather-tests.el ends here
