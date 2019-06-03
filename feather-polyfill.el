;;; feather-polyfill.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@conao-imac.local>
;; Keywords: lisp, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For legacy Emacs
;;

(unless (fboundp 'gnutls-available-p)
  (defun gnutls-available-p ()
    "Available status for gnutls.
(It is quite difficult to implement, so always return nil when not defined.
see `gnutls-available-p'.)"
    nil))

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory
    (if load-file-name
        (expand-file-name (file-name-directory load-file-name))
      "~/.emacs.d/")))

(unless (fboundp 'locate-user-emacs-file)
  (defun locate-user-emacs-file (name)
    "Simple implementation of `locate-user-emacs-file'."
    (format "%s%s" user-emacs-directory name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General functions
;;

(defsubst feather-message (from-fn msg &optional level)
  "Show message as MSG from FROM-FN.
LEVEL is one of :emargency, :error, :warning, :debug."
  (if level
      (display-warning 'feather (format "%s: %s" from-fn msg) level)
    (message (format "[feather] %s: %s" from-fn msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defun feather-list-all (lst)
  "Return t if all element in LST is non-nil, else nil."
  (let (result)
    (mapc (lambda (x) (setq result (and result x))) lst) result))

(defun feather-list-any (lst)
  "Return t if any elemetn in LST is non-nil, else nil."
  (let (result)
    (mapc (lambda (x) (setq result (or result x))) lst) result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General alist functions
;;

(defun feather-alist-get (alist key &optional default)
  "Look up KEY in ALIST, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (if (assoc key alist)
      (cdr (assoc key alist))
    default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General plist functions
;;

(defun feather-plist-get (plist key &optional default)
  "Look up KEY in PLIST, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (if (plist-member plist key)
      (plist-get plist key)
    default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General hash table functions
;;

(defsubst feather-ht-get (table key &optional default)
  "Look up KEY in TABLE, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (gethash key table default))

(defun feather-ht-map (fn table)
  "Apply FN to each key-value pair of TABLE, and make a list of the results.
FUNCTION is called with two arguments, KEY and VALUE."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall fn key value) results))
     table)
    results))

(defun feather-ht-keys (table)
  "Return a list of all the keys in TABLE."
  (feather-ht-map (lambda (key _value) key) table))

(defun feahter-ht-equal? (table1 table2)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Does not compare equality predicates."
  (let ((keys1 (feather-ht-keys table1))
        (keys2 (feather-ht-keys table2)))
    (and (equal (length keys1) (length keys2))
         (feather-list-all
          (mapcar (lambda (key)
                    (equal (feather-ht-get table1 key)
                           (feather-ht-get table2 key)))
                  keys1)))))

(defun feather-ht-update! (table from-table)
  "Update TABLE according to every key-value pair in FROM-TABLE."
  (maphash
   (lambda (key value) (puthash key value table))
   from-table)
  nil)

(defun feather-ht-merge (&rest tables)
  "Crete a new tables that includes all the key-value pairs from TABLES.
If multiple have tables have the same key, the value in the last
table is used."
  (let ((merged (make-hash-table :test 'eq)))
    (mapc (lambda (table) (feather-ht-update! merged table)) tables)
    merged))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General path functions
;;

(defun feather-path-parent (path)
  "Return parent path of PATH."
  (file-name-directory (directory-file-name path)))

(provide 'feather-polyfill)
;;; feather-polyfill.el ends here
