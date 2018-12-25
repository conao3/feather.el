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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((fn (lambda (x) (format "https://raw.githubusercontent.com/conao3/feather-recipes/master/feather-%s.el" x))))
    `((melpa                    . ,(funcall fn "recipes-melpa"))
      (melpa-stable             . ,(funcall fn "recipes-melpa_stable"))

      (melpa-detail             . ,(funcall fn "detail-melpa"))
      (melpa-stable-detail      . ,(funcall fn "detail-melpa_stable"))


      (melpa-list               . ,(funcall fn "recipes-melpa-list"))
      (melpa-stable-list        . ,(funcall fn "recipes-melpa_stable-list"))

      (melpa-detail-list        . ,(funcall fn "detail-melpa-list"))
      (melpa-stable-detail-list . ,(funcall fn "detail-melpa_stable-list"))

      (lite                     . ,(funcall fn "recipes-lite"))
      (lite-detail              . ,(funcall fn "detail-lite"))
      (lite-list                . ,(funcall fn "recipes-lite-list"))
      (lite-detail-list         . ,(funcall fn "detail-lite-list"))))
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
This variable is fed automatically by feather.el when installing a new package.
This variable is used by `feather-autoremove' to decide
which packages are no longer needed.

You can use it to (re)install packages on other machines
by running `feather-install-selected-packages'.

To check if a package is contained in this list here,
use `feather-user-selected-p'."
  :type '(repeat symbol)
  :group 'feather)

(defcustom feather-pinned-packages nil
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

(defvar feather-installed-alist nil
  "List of all packages user installed.
This variable is controlled by `feather-install' and `feather-remove'.")

(defvar feather-user-installed-alist nil
  "List of all packages user specifyed installed (without dependencies).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Anaphoric macros
;;

(defmacro feather-asetq (sym* &optional body)
  "Anaphoric setq macro.
\(fn (ASYM SYM) &optional BODY)"
  (declare (indent 1))
  `(let ((,(car sym*) ,(cadr sym*)))
     (setq ,(cadr sym*) ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General functions
;;

(defsubst feather-truep (var)
  "Return t if var is non-nil."
  (not (not var)))

(defsubst feather-message (from-fn msg &optional level)
  "Show message.
LEVEL is one of :emargency, :error, :warning, :debug."
  (if level
      (display-warning 'feather (format "%s: %s" from-fn msg) level)
    (message (format "[feather] %s: %s" from-fn msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Hash table util
;;

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
;;  Shell controllers
;;

(defun feather-command-queue (pkg cmdlst)
  "Execute cmdlst(string-list) queue with `start-process'.

CMDLST is like ((\"pwd\") (\"echo\" \"$(whoami)\")).
CMDLST will be escaped (\"pwd\" \"echo \\\\$\\\\(whoami\\\\)\").

The arguments passed in are properly escaped, so address vulnerabilities
like OS command injection.
The case, user can't get user-name (just get \\$(shoami)).

If CMDLST is (A B C), if A fails, B and subsequent commands will not execute."
  (let* ((safe-cmdlst (mapcar
                       (lambda (x)
                         (mapconcat #'shell-quote-argument x " "))
                       cmdlst))
         (command     (mapconcat #'identity safe-cmdlst " && "))
         (buffer-name (format "*feather-async-%s-%s*" pkg (gensym)))
         (buffer      (get-buffer-create buffer-name))
         (directory   default-directory)
         (proc        (get-buffer-process buffer)))

    (when (get-buffer-process buffer)
      (setq buffer (generate-new-buffer buffer-name)))
    
    (with-current-buffer buffer
      (shell-command-save-pos-or-erase)
      (setq default-directory directory)
      (setq proc (start-process buffer-name
                                buffer
                                shell-file-name      ; /bin/bash (default)
				shell-command-switch ; -c (default)
                                command))
      (setq mode-line-process '(":%s"))
      (require 'shell) (shell-mode)
      (set-process-sentinel proc 'shell-command-sentinel)
      ;; Use the comint filter for proper handling of
      ;; carriage motion (see comint-inhibit-carriage-motion).
      (set-process-filter proc 'comint-output-filter)
      (display-buffer buffer '(nil (allow-no-window . t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  advice
;;

(defun feather-show-download-progress (_fmt _perc &rest _args)
  "show download progress.
`:before' advice for `url-display-percentage'"
  (setq-local url-show-status t))

(defun feather-advice-add (&rest args)
  "advice-add after check `advice-add' defined.

\(fn SYMBOL WHERE FUNCTION &optional PROPS)"
  (when (fboundp 'advice-add)
    (apply #'advice-add args)))

(defun feather-advice-remove (&rest args)
  "advice-remove after check `advice-remove' defined.

\(fn SYMBOL FUNCTION)"
  (when (fboundp 'advice-remove)
    (apply #'advice-remove args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Git controllers
;;

;; (feather-git-clone-head "melpa" "https://github.com/melpa/melpa" feather-recipes-dir)
(defun feather-git-clone-head (pkg remote-url destdir)
  "Clone REMOTE-URL repository HEAD to DESTDIR. (shallow-clone)"
  (let ((destpath (concat destdir (file-name-nondirectory remote-url))))
    (if (file-directory-p destpath)
        (feather-git-pull-head pkg destpath)
      (let ((default-directory (expand-file-name destdir)))
        (feather-command-queue
         pkg
         `(("pwd")
           ("git" "clone" "-depth" "1" ,remote-url)))))))

;; (feather-git-clone-specific "https://github.com/conao3/cort.el"
;;                             "v0.1" feather-repos-dir)
(defun feather-git-clone-specific (pkg remote-url spec destdir)
  "Clone REMOTE-URL repository SPEC only to DESTDIR. (shallow-clone)"
  (let* ((repo-name (file-name-nondirectory remote-url))
         (destpath  (concat destdir repo-name)))
    (if (file-directory-p destpath)
        (let ((default-directory (expand-file-name destpath)))
          (feather-command-queue
           pkg
           `(("pwd")
             ("echo" "Repostory is already existed.")
             ("echo")
             ("echo" "If you want to check out to another commit,")
             ("echo" "first delete repository by `remove-package'."))))
      (let ((default-directory (expand-file-name destdir)))
        (feather-command-queue
         pkg
         `(("pwd")
           ("mkdir" ,repo-name)
           ("cd" ,repo-name)
           ("git" "init")
           ("git" "remote" "add" "origin" ,remote-url)
           ("git" "fetch" "-depth" "1" "origin" ,spec)
           ("git" "reset" "-hard" "FETCH_HEAD")))))))

;; (feather-git-pull-head (concat feather-recipes-dir "melpa"))
(defun feather-git-pull-head (pkg destpath)
  "Pull repository"
  (let ((default-directory (expand-file-name destpath)))
    (feather-command-queue
     pkg
     `(("pwd")
       ("git" "pull" "origin" "master")))))

(defun feather-git-unshalow (pkg destpath)
  "Unshallow repository to fetch whole repository.

see https://stackoverflow.com/questions/37531605/how-to-test-if-git-repository-is-shallow"
  (let ((default-directory (expand-file-name destpath)))
    (feather-command-queue
     pkg
     `(("pwd")
       ("git" "fetch" "-unshallow")
       ("git" "checkout" "master")))))
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interactive functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General manage packages
;;

;;;###autoload
(defun feather-package-installed-p (pkg)
  (feather-truep
   (assoc (intern pkg) feather-installed-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Remove packages
;;

;;;###autoload
(defun feather-autoremove ()
  "Remove packages that are no more needed.
Packages that are no more needed by other packages in
`feather-selected-packages-list' and their dependencies will be deleted."
  (interactive)
  (feather-initialize)
  
  (let ((lst (feather-install-selected-packages)))
    (mapc (lambda (x) (delq x lst) feather-selected-packages-list))
    (mapc (lambda (x) (feather-remove x)) lst)))

;;;###autoload
(defun feather-remove (pkg)
  "Remove specified package named PKG.
If you want to remove packages no more needed, call `feather-autoremove'."
  (interactive)
  (feather-initialize)
  )

;;;###autoload
(defun feather-clean ()
  "Clean feather working directory and build directory."
  (interactive)
  (feather-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Install packages
;;

;;;###autoload
(defun feather-install-selected-packages ()
  "Install `feather-selected-packages-list' listed packages."
  (interactive)
  (feather-initialize)
  
  (mapc (lambda (x) (feather-install x)) feather-selected-packages-list))

;;;###autoload
(defun feather-install (pkg)
  "Install specified package named PKG."
  (interactive)
  (feather-initialize)

  ;; remove old package if installed.
  (when feather-package-installed-p pkg
        (feather-remove pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Manage recipes
;;

;;;###autoload
(defun feather-refresh (&optional cache-p)
  "Reflesh package recipes specified `feather-fetcher-list'.
The URL corresponding to the symbol is managed with `feather-fetcher-url-alist'."
  (interactive)
  (feather-initialize)
  
  ;; clear all recipes.
  (setq feather-recipes (make-hash-table :test 'eq))

  ;; add advice to show progress
  (feather-advice-add 'url-display-percentage :before #'feather-show-download-progress)

  ;; download recipe files, read, append, save it.
  (mapc (lambda (x)
          (let* ((var-sym (intern (format "feather-recipes--%s" x)))
                 (filepath (format "%s%s.el" feather-recipes-dir var-sym)))

            ;; define recipe var
            (eval `(defvar ,var-sym))

            ;; load, or donload recipe if not recipe exist or chache-p is nil
            (condition-case err
                (progn
                  (with-temp-file filepath
                    (if (or (not cache-p) (not (file-exists-p filepath)))
                        (url-insert-file-contents
                         (cdr (assoc x feather-fetcher-url-alist)))
                      (insert-file-contents filepath))

                    ;; read file contents as sexp
                    ;; TODO: read from list support for Emacs-22
                    (eval `(setq ,var-sym (read (buffer-string)))))

                  ;; merge to main recipes var
                  (feather-asetq (it feather-recipes)
                    (feather-ht-merge it (eval var-sym))))
              (error
               (feather-message 'feather-refresh err :error)))))
        ;; updated after elements. first value will adoped.
        (reverse feather-fetcher-list))

  ;; remove advice
  (feather-advice-remove 'url-display-percentage #'feather-show-download-progress)

  ;; show status
  (feather-message 'feather-refresh
                   (format "Completed! %s recipes available."
                           (hash-table-count feather-recipes))))

;;;###autoload
(defun feather-list-packages ()
  "Show available packages list."
  (interactive)
  (feather-initialize)
  )

;;;###autoload
(defun feather-package-info (pkg)
  "Show package info."
  (interactive)
  (feather-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initialize feather
;;

;;;###autoload
(defun feather-initialize ()
  "Initialize `feather'"
  (interactive)
  (unless feather-initialized
    ;; create dir
    (mapc (lambda (x) (unless (file-directory-p x)
                        (make-directory x t)))
          `(,feather-repos-dir ,feather-recipes-dir ,feather-build-dir))

    ;; add load-path
    (add-to-list 'load-path feather-build-dir)

    ;; initialized frg
    (setq feather-initialized t)))

(provide 'feather)
;;; feather.el ends here
