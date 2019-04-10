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
                (feather-async-command-queue buf '(("echo" "test")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "test1\ntest2\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf '(("echo" "test1")
                                                   ("echo" "test2")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "/tmp\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf '(("cd" "/tmp")
                                                   ("pwd")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))

    (:string= "$(whoami)\n"
              (let ((buf (format "*feather-%sq*"
                                 (shell-command-to-string "uuidgen"))))
                (feather-async-command-queue buf '(("echo" "$(whoami)")))
                (sit-for 0.01)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  simple test
;;

(provide 'feather-tests)
;;; feather-tests.el ends here
