;; -*- lexical-binding: t -*-

(use-package rust-mode
  :straight t
  :init
  (setq rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))
