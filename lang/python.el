;; -*- lexical-binding: t -*-

(use-package python
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'tree-sitter-mode)
  (add-hook 'python-mode-hook 'tree-sitter-hl-mode)
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  )

(use-package pyvenv
  ;; virtualenv
  :straight t
  :commands pyvenv-activate
  )
