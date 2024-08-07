;; -*- lexical-binding: t -*-

(defun +fate/c-mode-common-hook ()
  (progn (eglot-ensure)
	 (yas-minor-mode-on)
	 (tree-sitter-mode)
	 (tree-sitter-hl-mode)
	 (outline-minor-mode)))

(defun +fate/add-clang-format-on-save ()
  (add-hook 'before-save-hook
	    (lambda () (clang-format-buffer))
	    nil
	    ;; buffer local hook
	    t))

(use-package cc-mode
  :init
  (setq c-basic-offset 4
	c-indentation-style "user"
	c-syntactic-indentation nil)
  :config
  (add-hook 'c-mode-hook '+fate/c-mode-common-hook)
  (add-hook 'c++-mode-hook '+fate/c-mode-common-hook)
  (add-hook 'c++-mode-hook '+fate/add-clang-format-on-save)
  )

(use-package clang-format
  :straight t
  :init
  (setq clang-format-style "file"
	clang-format-fallback-style "webkit"))
