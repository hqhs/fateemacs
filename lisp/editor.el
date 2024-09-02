;; -*- lexical-binding: t -*-

(defun +fate/treesit-install-grammars ()
  ;; emacs segfails on macos using "goto definition" with treesit enabled
  ;; debug, maybe?
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
	   '((c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.20.0"))
	     (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
	     ))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(use-package tree-sitter-langs
  :straight t
  )

(use-package tree-sitter
  :straight t
  :config
  (require 'tree-sitter-langs))

(use-package editorconfig
  :straight t
  :init
  (editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :straight t
  :config
  ;; TODO: what is smie mode?
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t
        ;; reduced from the default of 5000
        dtrt-indent-max-lines 2000)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

(use-package ws-butler
  :straight t
  :init
    ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.

  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

(defun +fate/sp-escape-and-remove-overlay ()
  "Combine sp-remove-active-pair-overlay and +fate/sp-escape-and-evil-escape."
  (interactive)
  (sp-remove-active-pair-overlay)
  (when (and (bound-and-true-p evil-mode)
             (bound-and-true-p evil-escape-mode)
             (eq evil-state 'insert))
    (evil-escape)))

(use-package smartparens
  :straight t
  ;; :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (smartparens-global-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)
  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; if inside a pair, smartparens remaps C-g to 'sp-remove-active-pair-overlay
  ;; forcing the user to press C-g twice to exit insert mode, which is annoying.
  ;; the following line fixes that.
  (define-key sp-pair-overlay-keymap (kbd "C-g") #'fate/sp-escape-and-remove-overlay)
  ;; (advice-add 'sp-remove-active-pair-overlay :after #'+fate/sp-escape-and-evil-escape)

  (defvar fate-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
	     (defun fate-enable-smartparens-mode-maybe-h ()
	       (when fate-buffer-smartparens-mode
		 (turn-on-smartparens-mode)
		 (kill-local-variable 'fate-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
	     (defun fate-disable-smartparens-mode-maybe-h ()
	       (when smartparens-mode
		 (setq-local fate-buffer-smartparens-mode t)
		 (turn-off-smartparens-mode))))
  )


(use-package pcre2el
  :straight t
  :commands (rxt-quote-pcre))
