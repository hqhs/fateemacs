;; -*- lexical-binding: t -*-

;;; Load vendored dependencies from vendor/
;;; No package manager. No network. Just load-path.

(defvar fate-vendor-dir (expand-file-name "vendor/" fate-emacs-dir)
  "Directory containing vendored dependencies.")

(dolist (dir (directory-files fate-vendor-dir t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;; use-package is built-in since Emacs 29. No :straight, no :ensure needed
;; for vendored packages -- they're already on load-path.
(require 'use-package)
