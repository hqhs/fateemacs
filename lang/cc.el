;; -*- lexical-binding: t -*-

(defun +fate/c-mode-common-hook ()
  (progn (eglot-ensure)
	 (tree-sitter-mode)
	 (tree-sitter-hl-mode)))

(use-package cc-mode
  :config
  (add-hook 'c-mode-hook '+fate/c-mode-common-hook)
  (add-hook 'c++-mode-hook '+fate/c-mode-common-hook))

(use-package clang-format
  :straight t
  :init
  (setq clang-format-style "file"
	clang-format-fallback-style "webkit"))
