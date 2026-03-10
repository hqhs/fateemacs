;; -*- lexical-binding: t -*-

(use-package python
  :config
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local +fate-format-command '("ruff" "format" "-")))))

(use-package pyvenv
  ;; virtualenv
  :commands pyvenv-activate)
