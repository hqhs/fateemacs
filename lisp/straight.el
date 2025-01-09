;; -*- lexical-binding: t -*-

;;; load package manager of choice: straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Verify package signatures when possible
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; Use HTTPS for package downloads
(setq straight-recipes-gnu-elpa-use-mirror t)

;; Configure use-package to use straight.el by default
;;(use-package straight
  ;;:custom (straight-use-package-by-default t))
