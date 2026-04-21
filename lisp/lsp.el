;; -*- lexical-binding: t -*-

(use-package eldoc
  :ensure nil ;; built-in
  :init
  (setq eldoc-echo-area-use-multiline-p nil
	eldoc-echo-area-display-truncation-message nil
	;; don't spam in minibuffer, display only if I asked to
	;; (kbd "K") to display doc for thing at point
	eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package flymake
  :ensure nil ;; built-in
  :init
  (setq flymake-no-changes-timeout 1.0
        flymake-start-on-save-only nil))

(use-package eglot
  :ensure nil ;; built-in
  :commands (eglot eglot-ensure)
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-stay-out-of nil
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
          (c++-mode . ("clangd"
                       "--background-index"
                       "--clang-tidy=false"
                       "--completion-style=detailed"
                       "--header-insertion=never"
                       "-j=4"))
          (c-mode . ("clangd"
                     "--background-index"
                     "--clang-tidy=false"
                     "--completion-style=detailed"
                     "--header-insertion=never"
                     "-j=4"))
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
        (eglot-inlay-hints-mode -1)
        (flymake-mode 1)
        (eldoc-mode -1))))

  ;; Optional: Add any additional customization
  :custom
  (eglot-connect-timeout 30)
  (eglot-sync-connect 3)
  (eglot-autoreconnect t))
