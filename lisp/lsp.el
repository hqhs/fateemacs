;; -*- lexical-binding: t -*-

(use-package eldoc
  :init
  (setq eldoc-echo-area-use-multiline-p nil
	eldoc-echo-area-display-truncation-message nil
	;; don't spam in minibuffer, display only if I asked to
	;; (kbd "K") to display doc for thing at point
	eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package eglot
  :commands (eglot eglot-ensure)
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-stay-out-of '(flymake)
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider
                                            :documentFormattingProvider
                                            :documentRangeFormattingProvider
                                            :documentOnTypeFormattingProvider
                                            :colorProvider
                                            :foldingRangeProvider))
  :config
  ;; Define server programs with all options preserved
  (setq eglot-server-programs
        `(;; C/C++ with preserved options
          (c++-mode . ("clangd" "--header-insertion=never"))
          (c-mode . ("clangd" "--header-insertion=never"))
          ;; Preserve any other language servers from eglot defaults
          ,@(cl-remove-if (lambda (entry)
                           (and (listp (car entry))
                                (or (member 'c++-mode (car entry))
                                    (eq (car entry) 'c-mode))))
                         eglot-server-programs)))

  ;; Hooks setup
  :hook
  ((eglot-managed-mode
    . (lambda ()
        ;; Disable inlay hints
        (eglot-inlay-hints-mode -1)
        ;; Disable automatic eldoc but keep eglot's eldoc setup
        ;; This preserves the ability to manually trigger docs with eldoc
        (eldoc-mode -1))))

  ;; Optional: Add any additional customization
  :custom
  (eglot-connect-timeout 30)
  (eglot-sync-connect nil)
  (eglot-autoreconnect t))
