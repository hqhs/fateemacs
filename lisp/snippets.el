;; -*- lexical-binding: t -*-

;; Yasnippet: snippet expansion and LSP placeholder support.
;; Snippets live in snippets/ directory at the project root.

(use-package yasnippet
  :ensure nil ;; vendored
  :hook (prog-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" fate-emacs-dir)))
  (yas-verbosity 1)
  ;; Don't expand snippets from completion-at-point (keyword triggers).
  ;; Yasnippet is used for LSP placeholders and manual expansion only.
  (yas-key-inhibit-hook-fn nil)
  :config
  ;; Keep abbrev-mode for abbreviations alongside yasnippet
  (add-hook 'prog-mode-hook #'abbrev-mode))

(provide 'fate-snippets)
