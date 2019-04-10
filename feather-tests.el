;;; feather-tests.el ---                             -*- lexical-binding: t; -*-

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

;;; Code:
(require 'feather)
(require 'cort-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

(cort-deftest feather:async-command-queue
  `((:string= "test\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf
                  '(("echo" "test")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "test1\ntest2\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf
                  '(("echo" "test1")
                    ("echo" "test2")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "/tmp\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf
                  '(("cd" "/tmp")
                    ("pwd")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "$(whoami)\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf
                  '(("echo" "$(whoami)")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))))

(cort-deftest feather:save-and-load-variables
  `((:string= "(setq feather-installed-list
      '(zzz-to-char))
(setq feather-selected-packages-list
      '(zzz-to-char))
(setq feather-pinned-packages-alist
      '((zzz-to-char . melpa)))
"
              (let ((feather-installed-list         '(zzz-to-char))
                    (feather-selected-packages-list '(zzz-to-char))
                    (feather-pinned-packages-alist  '((zzz-to-char . melpa))))
                (with-temp-buffer
                  (insert-file-contents (feather-save-data))
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:equal '((feather-installed-list         '(zzz-to-char))
              (feather-selected-packages-list '(zzz-to-char))
              (feather-pinned-packages-alist  '((zzz-to-char . melpa))))
            (let ((feather-installed-list         '(zzz-to-char))
                  (feather-selected-packages-list '(zzz-to-char))
                  (feather-pinned-packages-alist  '((zzz-to-char . melpa)))
                  (path))
              (setq path (feather-save-data))
              (let ((feather-installed-list         nil)
                    (feather-selected-packages-list nil)
                    (feather-pinned-packages-alist  nil))
                (load-file path)
                `((feather-installed-list         ',feather-installed-list)
                  (feather-selected-packages-list ',feather-selected-packages-list)
                  (feather-pinned-packages-alist  ',feather-pinned-packages-alist)))))))

(cort-deftest feaher:fetch-and-load-recipe
  `((:string= "#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data 
   (zzz-to-char (:fetcher \"github\" :repo \"mrkkrp/zzz-to-char\" :files nil)
    zygospore (:fetcher \"github\" :repo \"LouisKottmann/zygospore.el\" :files nil)
    zweilight-theme (:fetcher \"github\" :repo \"philiparvidsson/Zweilight-Theme-for-Emacs\" :files nil)
    ztree (:fetcher \"github\" :repo \"fourier/ztree\" :files nil)
    zprint-mode (:fetcher \"github\" :repo \"pesterhazy/zprint-mode.el\" :files nil)
   ))
"
              (with-temp-buffer
                (insert-file-contents
                 (feather-fetch-recipe
                  "lite"
                  "https://raw.githubusercontent.com/conao3/feather-recipes.el/master/recipes/lite.el"))
                (buffer-substring-no-properties (point-min) (point-max))))
    (:feahter-ht-equal? #s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data
                                      (zzz-to-char (:fetcher "github" :repo "mrkkrp/zzz-to-char" :files nil)
                                                   zygospore (:fetcher "github" :repo "LouisKottmann/zygospore.el" :files nil)
                                                   zweilight-theme (:fetcher "github" :repo "philiparvidsson/Zweilight-Theme-for-Emacs" :files nil)
                                                   ztree (:fetcher "github" :repo "fourier/ztree" :files nil)
                                                   zprint-mode (:fetcher "github" :repo "pesterhazy/zprint-mode.el" :files nil)))
                        (progn
                          (feather-fetch-recipe
                           "lite"
                           "https://raw.githubusercontent.com/conao3/feather-recipes.el/master/recipes/lite.el")
                          (feather-load-recipe "lite")))))

(cort-deftest feather:refresh-recipes
  `((:feahter-ht-equal? #s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data
                                      (zzz-to-char (:fetcher "github" :repo "mrkkrp/zzz-to-char" :files nil)
                                                   zygospore (:fetcher "github" :repo "LouisKottmann/zygospore.el" :files nil)
                                                   zweilight-theme (:fetcher "github" :repo "philiparvidsson/Zweilight-Theme-for-Emacs" :files nil)
                                                   ztree (:fetcher "github" :repo "fourier/ztree" :files nil)
                                                   zprint-mode (:fetcher "github" :repo "pesterhazy/zprint-mode.el" :files nil)))
                        (let ((feather-recipes nil)
                              (feather-fetcher-list '(lite)))
                          (feather-refresh)
                          feather-recipes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  simple test
;;

(provide 'feather-tests)
;;; feather-tests.el ends here
