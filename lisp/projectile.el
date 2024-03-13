;; -*- lexical-binding: t -*-

(defun +fate-project-ignored-p (project-root)
  "Return non-nil if temporary file or a straight package."
  (unless (file-remote-p project-root)
    (file-in-directory-p project-root (expand-file-name "~/.cargo"))))


(use-package projectile
  :straight t
  :init
  (setq projectile-enable-caching t
	projectile-cache-file (concat fate-cache-dir "projectile.cache")
	projectile-known-projects-file (concat fate-cache-dir "projectile.projects")
	projectile-ignored-project-function #'+fate-project-ignored-p)
  :config
  (projectile-mode 1))
