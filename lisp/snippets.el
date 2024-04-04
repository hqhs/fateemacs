;; -*- lexical-binding: t -*-

(defvar +snippets-dir (expand-file-name "snippets/" fate-emacs-dir))

(use-package yasnippet
  :straight t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)

  :config

  (add-to-list 'yas-snippet-dirs '+snippets-dir)

  (yas-global-mode +1))
