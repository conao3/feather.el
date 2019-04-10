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

(require 'feather-polyfill)

(defgroup feather nil
  "Emacs package manager with parallel processing."
  :group 'lisp)

(defconst feather-version "0.0.1"
  "feather.el version")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizable variables
;;

(defcustom feather-fetcher-list '(melpa)
  "A list of sites to fetch.
If there are multiple download destinations,
priority is given to the site located at the head of the list.

see `feather-fetcher-url-alist' available fetcher symbol"
  :type 'sexp
  :group 'feather)

(defcustom feather-fetcher-detail-list '(melpa-detail)
  "A list of sites to fetch detail recipe file. Use at `feather-list-packages'.

see `feather-fetcher-url-alist' available fetcher symbol"
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
  "All directories feather managed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Package configuration
;;

(defcustom feather-user-recipes-hash-table nil
  "User defined package recipes hash table. Overrides any recipes.
Recipe need `:repo', [`:fetcher'], [`:commit'], [`:files']. see `feather-recipes'.

If you omit `:fetcher', install from GitHub.
If you omit `:commit', install HEAD.
If you omit `:files', install `:defaults' file see `feather-package-defaults-files'

Sample:
#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data
   (zzz-to-char     (:fetcher \"github\" :repo \"mrkkrp/zzz-to-char\" :files nil)
    zygospore       (:repo \"LouisKottmann/zygospore.el\" :commit \"0.0.3\")
    ztree           (:repo \"fourier/ztree\" :commit \"c54425a094353ec40a\")
    zweilight-theme (:repo \"philiparvidsson/Zweilight-Theme-for-Emacs\")))"
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

(defconst feather-package-defaults-files
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.

see `package-build-default-files-spec' from
https://github.com/melpa/melpa/blob/master/package-build/package-build.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shell controllers
;;

(defun feather-async-command-queue (buffer-name cmdlst)
  "Execute cmdlst(string-list) queue with `start-process'.

Command output is appear in generated buffer named BUFFER-NAME.

CMDLST is like ((\"pwd\") (\"echo\" \"$(whoami)\")).
CMDLST will be escaped (\"pwd\" \"echo \\\\$\\\\(whoami\\\\)\").

The arguments passed in are properly escaped, so address vulnerabilities
like OS command injection.
The case, user can't get user-name (just get \\$(whoami)).

If CMDLST is (A B C), if A fails, B and subsequent commands will not execute.

This function inspired by `shell-command'"
  (let* ((safe-cmdlst (mapcar
                       (lambda (x)
                         (mapconcat #'shell-quote-argument x " "))
                       cmdlst))
         (command     (mapconcat #'identity safe-cmdlst " && "))
         (proc))
    (with-current-buffer (generate-new-buffer buffer-name)
      (setq proc (start-process (buffer-name)
                                (current-buffer)
                                shell-file-name      ; /bin/bash (default)
				shell-command-switch ; -c (default)
                                command))
      (setq mode-line-process '(":%s"))
      (require 'shell) (shell-mode)
      (set-process-sentinel proc 'shell-command-sentinel)
      ;; Use the comint filter for proper handling of
      ;; carriage motion (see comint-inhibit-carriage-motion).
      (set-process-filter proc 'comint-output-filter)
      (display-buffer (current-buffer) '(nil (allow-no-window . t))))))

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
;; (feather-git-clone "feather.el" "https://github.com/conao3/feather.el.git" feather-recipes-dir)
(defun feather-git-full-clone (pkg url dir)
  "Clone PKG repository from URL on DIR. (full-clone)"
  (let ((repodir (expand-file-name "" dir)))
    (unless (file-directory-p (expand-file-name pkg dir))
      (feather-async-command-queue
       (format "*feather-async-%s-%s*" pkg (gensym))
       `(("echo" ,(format "[Clone] '%s'... " pkg))
         ("mkdir" "-p" ,repodir)
         ("cd" ,repodir)
         ("pwd")
         ("git" "clone" ,url)
         ("echo" ,(format "[Clone] '%s' done" pkg)))))))

(defun feather-git-shallow-clone (pkg url id dir)
  "Clone PKG repository from URL on DIR. (shallow-clone)

ID requires an id that can specify the repository tree such as
\"master\" (branch-name), \"v1.2\" (tag-name), \"fc697e2a9...e86\" (SHA-1)

See https://yo.eki.do/notes/git-only-single-commit ."
  (let ((repodir (expand-file-name "" dir)))
    (unless (filie-directory-p (expand-file-name pkg dif))
      (feather-async-command-queue
       (format "*feather-async-%s-%s*" pkg (gensym))
       `(("echo" ,(format "[Shallow clone] '%s'... " pkg))
         ("mkdir" "-p" ,repodir)
         ("cd" ,repodir)
         ("pwd")
         ("mkdir" pkg)
         ("cd" pkg)
         ("git" "init")
         ("git" "remote" "add" "origin" ,url)
         ("git" "fetch" "--depth" "1" ,id)
         ("git" "reset" "--hard" "FETCH_HEAD")
         ("echo" ,(format "[shallow clone] '%s' done" pkg)))))))

;; (feather-git-pull-head (concat feather-recipes-dir "melpa"))
;; (defun feather-git-pull-head (pkg destpath)
;;   "Pull repository"
;;   (let ((default-directory (expand-file-name destpath)))
;;     (feather-async-command-queue
;;      pkg
;;      `(("pwd")
;;        ("git" "pull" "origin" "master")))))

(defun feather-git-unshalow (pkg dir)
  "Unshallow repository to fetch whole repository.

see https://stackoverflow.com/questions/37531605/how-to-test-if-git-repository-is-shallow"
  (let ((repodir (expand-file-name "" dir)))
    (when (and (file-directory-p (expand-file-name pkg dir))
               (file-exists-p (expand-file-name (concat pkg "/.git/shallow") dir)))
      (feather-async-command-queue
       (format "*feather-async-%s-%s*" pkg (gensym))
       `(("echo" ,(format "[Unshallow] '%s'... " pkg))
         ("mkdir" "-p" ,repodir)
         ("cd" ,repodir)
         ("pwd")
         ("git" "fetch" "--unshallow")
         ("echo" ,(format "[Unshallow] '%s' done " pkg)))))))

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

(defun feather-ensure-package (pkg)
  "ensure PKG."
  )

(defun feather-packages-list ()
  "Return available package name list"
  (feather-alet (it ((lst)))
    (maphash (lambda (key val) (push key lst)) feather-recipes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interactive functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General manage packages
;;

;;;###autoload
(defun feather-installed-package-info (pkg)
  "Return installed package info.
If package haven't installed yet, return nil.
If package have removed, return (:state :removed)"
  (let ((pkg* (intern pkg)))
    (plist-member feather-installed-plist pkg*)))

;;;###autoload
(defun feather-package-installed-p (pkg)
  (let ((pkg* (intern pkg)))
    (let ((info (feather-installed-package-info pkg)))
      (feather-truep
       (and info
            (not (eq (plist-get info :state) :removed)))))))

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
(defun feather-remove (pkg &optional force-p)
  "Remove specified package named PKG.
If you want to remove packages no more needed, call `feather-autoremove'."
  (interactive "nRemove package: ")
  (feather-initialize)

  (let ((pkg* (intern pkg)))
    (when (and (feather-package-installed-p pkg)
               (or force-p
                   (y-or-n-p (format "Really remove %s?" pkg))))
      (condition-case err
          (let ((info (feather-installed-package-info pkg)))
            ;; delete package build-files
            (mapc #'delete-file (plist-get info :build-files))
            
            ;; delete package source dir
            (delete-directory (concat feather-repos-dir pkg))

            ;; show info
            (feather-message 'feather-remove
                             "Complete remove. Refresh Emacs."))
        (error (feather-message 'feather-remove err :warning))))))

;;;###autoload
(defun feather-clean ()
  "Clean feather working directory and build directory."
  (interactive)
  (feather-initialize)
  (when (y-or-n-p "Really clean feather directory? All packages will delete.")
    (mapc (lambda (x) (delete-directory (eval x) t)) feather-dirs)
    
    ;; create raw directory
    (feather-initialize t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Install packages
;;

;;;###autoload
(defun feather-install-selected-packages ()
  "Install `feather-selected-packages-list' listed packages."
  (interactive)
  (feather-initialize)

  (when feather-selected-packages-list
    (mapc (lambda (x) (feather-install (symbol-name x))
            feather-selected-packages-list))))

;;;###autoload
(defun feather-install (pkg)
  "Install specified package named PKG."
  (interactive "nInstall package: ")
  (feather-initialize)

  (let* ((pkg* (intern pkg))
         (info (gethash pkg* feather-recipes))
    ;; remove old package if installed.
    (if (feather-package-installed-p pkg)
      (when (y-or-n-p (format "%s is already installed. Reinstall?" pkg))
        (feather-remove pkg)
        (feather-install pkg))

      ;; download source
      (feather-ensure-package pkg)

      ;; generate autoloads
      (feather-generate-autoloads pkg)

      ;; acrivate package
      (feather-activate pkg)))))

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
  (feather-packages-list))

;;;###autoload
(defun feather-package-info (pkg)
  "Show package info.

such as (feather-package-info :zzz-to-char)"
  (interactive)
  (feather-initialize)
  (gethash pkg feather-recipes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initialize feather
;;

;;;###autoload
(defun feather-save-data ()
  "Save `feather-installed-plist' (inner variable),
        `feather-selected-packages-list' (custom variable),
        `feather-pinned-packages-alist'  (custom variable)
TODO: list support for Emacs-22."
  (let ((filepath (concat feather-recipes-dir "feather-data.el")))
    (when (file-writable-p filepath)
          (with-temp-file filepath
            (mapc (lambda (x)
                    (insert (format "(defvar %s)\n(setq %s %s)\n\n"
                                    x x (eval x))))
                  '(feather-installed-plist
                    feather-selected-packages-list
                    feather-pinned-packages-alist))))))

;;;###autoload
(defun feather-load-data ()
  "Load `feather-installed-plist' (inner variable),
        `feather-selected-packages-list' (custom variable),
        `feather-pinned-packages-alist'  (custom variable)
TODO: list support for Emacs-22"
  (let ((filepath (concat feather-recipes-dir "feather-data.el")))
    (when (file-readable-p filepath)
      (load-file filepath))))

;;;###autoload
(defun feather-initialize (&optional force-p)
  "Initialize `feather'"
  (interactive)
  (when (or force-p (not feather-initialized))
    ;; create dirs
    (mapc (lambda (x)
            (unless (file-directory-p x)
              (make-directory x t)))
          feather-dirs)

    ;; add load-path
    (add-to-list 'load-path feather-build-dir)

    ;; load feather database
    (feather-load-data)

    ;; initialized frg
    (setq feather-initialized t)))

(provide 'feather)
;;; feather.el ends here
