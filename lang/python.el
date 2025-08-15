;; -*- lexical-binding: t -*-

(use-package python
  :config
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  )

(use-package pyvenv
  ;; virtualenv
  :straight t
  :commands pyvenv-activate
  )

(use-package conda
  :straight t
  :config
  ;; Set conda executable path if not in PATH
  ;; (setq conda-anaconda-home "/path/to/anaconda3")
  ;; (setq conda-env-home-directory "/path/to/anaconda3/envs")

  ;; Auto-activate conda environment based on project
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t)

  ;; Optional: set default environment
  ;; (conda-env-activate "base")
  )
