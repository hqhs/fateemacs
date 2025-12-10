;; -*- lexical-binding: t -*-

(use-package python
  :config
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))

  (add-hook 'python-ts-mode-hook #'apheleia-mode)

  (setf (alist-get 'ruff apheleia-formatters)
      '("ruff" "format" "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(ruff))
  )

(use-package pyvenv
  ;; virtualenv
  :straight t
  :commands pyvenv-activate
  )
