;;; feather.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: .emacs

;; This program is free software; you can redistribute it and/or modify
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

;;

(defgroup feather nil
  "Emacs package manager with parallel processing."
  :group 'lisp)

(defconst feather-version "0.0.1"
  "feather.el version")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For legacy Emacs
;;

(unless (fboundp 'gnutls-available-p)
  (defun gnutls-available-p () nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizable variables
;;

(defcustom leaf-archives `(("gnu" . ,(format "http%s://elpa.gnu.org/packages/"
                                          (if (gnutls-available-p) "s" ""))))
  "An alist of archives from which to fetch.
If there are multiple download destinations, value top of the list is adopted"
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (string :tag "URL or directory name"))
  :group 'feather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function
;;

(defun feather-install (package)
  "Install package."
  )

(provide 'feather)
;;; feather.el ends here
