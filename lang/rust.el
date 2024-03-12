;; -*- lexical-binding: t -*-

(defun +eglot-without-hints ()
  (progn (eglot-ensure)
         (message "eglot-ensure called")
         (eglot-inlay-hints-mode -1)))

(use-package rust-mode
  :straight t
  :init
  (setq rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))
