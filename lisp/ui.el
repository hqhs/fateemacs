;; -*- lexical-binding: t -*-

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-github nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil)
  :config
  (doom-modeline-mode))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-one t))
