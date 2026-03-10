;; -*- lexical-binding: t -*-

;;; Load vendored dependencies from vendor/
;;; No package manager. No network. Just load-path.

(defvar fate-vendor-dir (expand-file-name "vendor/" fate-emacs-dir)
  "Directory containing vendored dependencies.")

(dolist (dir (directory-files fate-vendor-dir t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    ;; evil-collection has modes/<name>/ subdirs that need to be on load-path
    (let ((modes-dir (expand-file-name "modes" dir)))
      (when (file-directory-p modes-dir)
        (dolist (subdir (directory-files modes-dir t "^[^.]"))
          (when (file-directory-p subdir)
            (add-to-list 'load-path subdir)))))))

;; use-package is built-in since Emacs 29. No :straight, no :ensure needed
;; for vendored packages -- they're already on load-path.
(require 'use-package)

;; CRITICAL: Never try to install anything. No network, no package.el.
(setq use-package-always-ensure nil)
(setq package-enable-at-startup nil)
(setq package-archives nil)
