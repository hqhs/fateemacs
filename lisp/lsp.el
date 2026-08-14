;; -*- lexical-binding: t -*-

(use-package eldoc
  :ensure nil ;; built-in
  :init
  (setq eldoc-echo-area-use-multiline-p nil
	eldoc-echo-area-display-truncation-message nil
	;; don't spam in minibuffer, display only if I asked to
	;; (kbd "K") to display doc for thing at point
	eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package yasnippet
  :ensure nil ;; vendored
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" fate-emacs-dir)))
  (yas-reload-all))

(use-package eglot
  :ensure nil ;; built-in
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
          ((c++-mode c++-ts-mode) . ("clangd"
                                     "--background-index"
                                     "--clang-tidy=false"
                                     "--completion-style=detailed"
                                     "--header-insertion=never"
                                     "-j=4"
                                     "--pch-storage=memory"))
          ((c-mode c-ts-mode) . ("clangd"
                                 "--background-index"
                                 "--clang-tidy=false"
                                 "--completion-style=detailed"
                                 "--header-insertion=never"
                                 "-j=4"))
          ;; Preserve any other language servers from eglot defaults
          ;; Entry keys come in three shapes: MODE, (MODE ...), and
          ;; ((MODE :language-id "...") ...). Normalise before comparing.
          ,@(cl-remove-if (lambda (entry)
                            (let* ((key (car entry))
                                   (modes (mapcar (lambda (m)
                                                    (if (consp m) (car m) m))
                                                  (if (listp key) key (list key)))))
                              (cl-intersection modes
                                               '(c-mode c-ts-mode
                                                 c++-mode c++-ts-mode))))
                          eglot-server-programs)))

  ;; clangd occasionally answers completion requests with -32602
  ;; "trying to get preamble for non-added document" while the preamble
  ;; is still building. The error is transient — swallow it so corfu
  ;; doesn't dump a backtrace into the echo area.
  (advice-add 'eglot-completion-at-point :around
              (lambda (orig &rest args)
                (condition-case err
                    (apply orig args)
                  (jsonrpc-error
                   (unless (string-match-p
                            "non-added document"
                            (or (alist-get 'jsonrpc-error-message (cdr err)) ""))
                     (signal (car err) (cdr err)))
                   nil))))

  ;; Hooks setup
  :hook
  ((eglot-managed-mode
    . (lambda ()
        (eglot-inlay-hints-mode -1)
        (eldoc-mode -1))))

  ;; Optional: Add any additional customization
  :custom
  (eglot-connect-timeout 30)
  (eglot-sync-connect 1)
  (eglot-autoreconnect t))
