;;; feather-variables.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@conao-imac.local>
;; Keywords: lisp

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizable variables
;;

(defcustom feather-fetcher-list '(melpa)
  "A list of sites to fetch.
If there are multiple download destinations,
priority is given to the site located at the head of the list

[TODO]: Now, support melpa only."
  ;; :type '(sexp
  ;;         (symbol
  ;;          ;; (const :tag "Elpa"         'elpa)
  ;;          (const :tag "Melpa"        'melpa)))
  ;;          ;; (const :tag "Melpa-stable" 'melpa-stable)
  ;;          ;; (const :tag "el-get"       'el-get)
  ;;          ;; (const :tag "cask"         'cask)))
  :type 'sexp
  :group 'feather)

(defcustom feather-fetcher-detail-list '(melpa-detail)
  "A list of sites to fetch detail recipe file.

[TODO]: Now, support melpa only."
  ;; :type '(sexp
  ;;         (symbol
  ;;          ;; (const :tag "Elpa"         'elpa)
  ;;          (const :tag "Melpa"        'melpa)))
  ;;          ;; (const :tag "Melpa-stable" 'melpa-stable)
  ;;          ;; (const :tag "el-get"       'el-get)
  ;;          ;; (const :tag "cask"         'cask)))
  :type 'sexp
  :group 'feather)

(defcustom feather-fetcher-url-alist
  (let ((fn (lambda (x) (format "https://raw.githubusercontent.com/conao3/feather-recipes/master/%s.el" x))))
    `((melpa                    . ,(funcall fn "recipes/melpa"))
      (melpa-stable             . ,(funcall fn "recipes/melpa_stable"))

      (melpa-detail             . ,(funcall fn "detail/melpa"))
      (melpa-stable-detail      . ,(funcall fn "detail/melpa_stable"))


      (melpa-list               . ,(funcall fn "recipes/melpa-list"))
      (melpa-stable-list        . ,(funcall fn "recipes/melpa_stable-list"))

      (melpa-detail-list        . ,(funcall fn "detail/melpa-list"))
      (melpa-stable-detail-list . ,(funcall fn "detail/melpa_stable-list"))

      (lite                     . ,(funcall fn "recipes/lite"))
      (lite-detail              . ,(funcall fn "detail/lite"))
      (lite-list                . ,(funcall fn "recipes/lite-list"))
      (lite-detail-list         . ,(funcall fn "detail/lite-list"))))
  "Fetcher URL alist. see `feather-fetcher-list'."
  :type 'alist
  :group 'feather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Directory paths
;;

(defcustom feather-repos-dir (locate-user-emacs-file "feather/repos/")
  "Directory where the download Emacs Lisp packages is placed."
  :type 'directory
  :group 'feather)

(defcustom feather-recipes-dir (locate-user-emacs-file "feather/recipes/")
  "Directory where the recipes is placed."
  :type 'directory
  :group 'feather)

(defcustom feather-build-dir (locate-user-emacs-file "feather/build/")
  "Directory where byte-compiled Emacs Lisp files is placed"
  :type 'directory
  :group 'feather)

(defvar feather-dirs '(feather-repos-dir feather-recipes-dir feather-build-dir)
  "All directorys feather managed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Package configuration
;;

(defcustom feather-user-recipes-hash-table nil
  "User defined package recipes hash table. Overrides any recipes.
Recipe need `:ver', `:deps', `:url', [`:commit']. see `feather-recipes'.
If you omit `:commit', install HEAD.

Sample:
#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data
   (:zprint-mode (
      :ver (20181111 1945)
      :deps (:emacs (24 3))
      :url \"https://github.com/pesterhazy/zprint-mode.el\"
      :description nil
      :keywords (\"tools\")
      :authors (\"Paulus Esterhazy <pesterhazy@gmail.com>\")
      :maintainer \"Paulus Esterhazy <pesterhazy@gmail.com>\")
    :ztree (
      :ver (20180512 1850)
      :deps nil
      :url \"https://github.com/fourier/ztree\"
      :commit \"a788db1d0faec7365cb743f62d015ce6c561b028\")))"
  :type 'sexp
  :group 'feather)

(defcustom feather-selected-packages-list nil
  "Store here packages installed explicitly by user.
This variable is must be list by quoted symbol.
This variable is fed automatically by feather.el when installing a new package.
This variable is used by `feather-autoremove' to decide
which packages are no longer needed.

You can use it to (re)install packages on other machines
by running `feather-install-selected-packages'.

To check if a package is contained in this list here,
use `feather-user-selected-p'."
  :type '(repeat symbol)
  :group 'feather)

(defcustom feather-pinned-packages-alist nil
  "An alist of packages that are pinned to specific archives.
This can be useful if you have multiple package archives enabled,
and want to control which archive a given package gets installed from.

Each element of the alist has the form (PACKAGE . ARCHIVE), where:
 PACKAGE is a symbol representing a package
 ARCHIVE is a string representing an archive (it should be element in
`feather-fetcher-list', e.g. 'melpa-stable).

Adding an entry to this variable means that only ARCHIVE will be
considered as a source for PACKAGE.  If other archives provide PACKAGE,
they are ignored (for this package).  If ARCHIVE does not contain PACKAGE,
the package will be unavailable."
  :type '(alist :key-type (symbol :tag "Package")
                :value-type (symbol :tag "Archive"))
  :group 'feather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inner variables - DON'T change!
;;

(defvar feather-initialized nil
  "Manage `feather' initialization state.
This variable is set automatically by `feather-initialize'.")

;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Manage Process

(defvar feather-process-state-alist nil
  "Manage `feather' process state.
When change process state changed, pushed new state.")

;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Manage recipes
;;

(defvar feather-recipes nil
  "Package recipes.
Stored ordered by `feather-fetcher-list'.
This variable is set automatically by `feather-initialize'.")

;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Manage packages

(defvar feather-installed-plist nil
  "List of all packages user installed.
This variable is controlled by `feather-install' and `feather-remove'.")

(provide 'feather-variables)
;;; feather-variables.el ends here
