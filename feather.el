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
;;  Customizable variables
;;

;; (defcustom feather-archives
;;   `(("gnu" . ,(format "http%s://elpa.gnu.org/packages/"
;;                       (if (gnutls-available-p) "s" ""))))
;;   "An alist of archives from which to fetch.
;; If there are multiple download destinations, value top of the list is adopted"
;;   :type '(alist :key-type (string :tag "Archive name")
;;                 :value-type (string :tag "URL or directory name"))
;;   :group 'feather)

(defcustom feather-working-dir (locate-user-emacs-file "feather-repos")
  "Directory is located download Emacs Lisp packages path."
  :type 'directory
  :group 'feather)

(defcustom feather-build-dir (locate-user-emacs-file "feather-build")
  "Directory is located byte-compiled Emacs Lisp files path."
  :type 'directory
  :group 'feather)

(defcustom feather-selected-packages nil
  "Store here packages installed explicitly by user.
This variable is fed automatically by feather.el when installing a new package.
This variable is used by `feather-autoremove' to decide
which packages are no longer needed.

You can use it to (re)install packages on other machines
by running `feather-install-selected-packages'.

To check if a package is contained in this list here,
use `feather-user-selected-p'."
  :type '(repeat symbol)
  :group 'feather)

;;
;; sample packages alist
;;
;; '((use-package
;;     ((:name use-package)
;;      (:version (20181119 2350))
;;      (:description "A configuration macro for simplifying your .emacs")
;;      (:dependencies ((emacs (24 3)) (bind-key (2 4))))
;;      (:dir "/Users/conao/.emacs.d/local/26.1/elpa/use-package-20181119.2350")
;;      (:url "https://github.com/jwiegley/use-package")
;;      (:maintainer ("John Wiegley" . "johnw@newartisans.com"))
;;      (:authors (("John Wiegley" . "johnw@newartisans.com")))
;;      (:keywords ("dotemacs" "startup" "speed" "config" "package"))))
;;   (shackle
;;    ((:name shackle)
;;     (:version (20171209 2201))
;;     (:description "Enforce rules for popups")
;;     (:dependencies ((cl-lib (0 5))))
;;     (:dir "/Users/conao/.emacs.d/local/26.1/elpa/shackle-20171209.2201")
;;     (:url "https://github.com/wasamasa/shackle")
;;     (:maintainer ("Vasilij Schneidermann" . "v.schneidermann@gmail.com"))
;;     (:authors (("Vasilij Schneidermann" . "v.schneidermann@gmail.com")))
;;     (:keywords ("convenience")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

(defvar package-alist nil
  "Alist of all packages available for activation.
This variable is set automatically by `package-initialize'.")

(defun feather-get-installed-packages ()
  "Return list of packages installed. Include dependencies packages."
  )

(defun feather-get-installed-packages-non-dependencies ()
  "Return list of packages installed by user's will."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Package contorollers
;;

(defun feather-activate (pkg)
  "Activate PKG with dependencies packages."
  )

(defun feather-generate-autoloads (pkg)
  "Generate autoloads .el file"
  )

(defun feather-buffer-info ()
  "Return a `package-desc' describing the package in the current buffer.

If the buffer does not contain a conforming package, signal an
error.  If there is a package, narrow the buffer to the file's
boundaries."
  (goto-char (point-min))
  (unless (re-search-forward "^;;; \\([^ ]*\\)\\.el ---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$" nil t)
    (error "Package lacks a file header"))
  (let ((file-name (match-string-no-properties 1))
        (desc      (match-string-no-properties 2))
        (start     (line-beginning-position)))
    (unless (search-forward (concat ";;; " file-name ".el ends here"))
      (error "Package lacks a terminating comment"))
    ;; Try to include a trailing newline.
    (forward-line)
    (narrow-to-region start (point))
    (require 'lisp-mnt)
    ;; Use some headers we've invented to drive the process.
    (let* ((requires-str (lm-header "package-requires"))
           ;; Prefer Package-Version; if defined, the package author
           ;; probably wants us to use it.  Otherwise try Version.
           (pkg-version
            (or (package-strip-rcs-id (lm-header "package-version"))
                (package-strip-rcs-id (lm-header "version"))))
           (homepage (lm-homepage)))
      (unless pkg-version
        (error
            "Package lacks a \"Version\" or \"Package-Version\" header"))
      (package-desc-from-define
       file-name pkg-version desc
       (if requires-str
           (package--prepare-dependencies
            (package-read-from-string requires-str)))
       :kind 'single
       :url homepage
       :maintainer (lm-maintainer)
       :authors (lm-authors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interactive functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Remove packages
;;

;;;###autoload
(defun feather-autoremove ()
  "Remove packages that are no more needed.
Packages that are no more needed by other packages in
`feather-selected-packages' and their dependencies will be deleted."
  (interactive)
  (let ((lst (feather-install-selected-packages)))
    (mapc (lambda (x) (delq x lst) feather-selected-packages))
    (mapc (lambda (x) (feather-remove x)) lst)))

;;;###autoload
(defun feather-remove (pkg)
  "Remove specified package named PKG.
If you want to remove packages no more needed, call `feather-autoremove'."
  (interactive)
  )

;;;###autoload
(defun feather-clean ()
  "Clean feather working directory and build directory."
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Install packages
;;

;;;###autoload
(defun feather-install-selected-packages ()
  "Install `feather-selected-packages' listed packages."
  (interactive)
  (mapc (lambda (x) (feather-install x)) feather-selected-packages))

;;;###autoload
(defun feather-install (pkg)
  "Install specified package named PKG."
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initialize packages
;;

;;;###autoload
(defun feather-initialize ()
  "Initialize selected packages."
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Manage packages
;;

;;;###autoload
(defun feather-refresh ()
  "Fetch package info list from `feather-archive'."
  (interactive)
  )

;;;###autoload
(defun feather-list-packages ()
  "Show available packages list."
  (interactive)
  )

;;;###autoload
(defun feather-package-info (pkg)
  "Show package info."
  (interactive)
  )

(provide 'feather)
;;; feather.el ends here
