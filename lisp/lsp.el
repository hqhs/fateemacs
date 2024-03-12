;; -*- lexical-binding: t -*-

(use-package eglot
  :commands (eglot eglot-ensure)
  :straight t
  :init
  (setq eglot-autoshutdown t
        ;; NOTE This setting disable the eglot-events-buffer enabling more
        ;;      consistent performance on long running emacs instance.
        ;;      Default is 2000000 lines. After each new event the whole buffer
        ;;      is pretty printed which causes steady performance decrease over time.
        ;;      CPU is spent on pretty priting and Emacs GC is put under high pressure.
        eglot-events-buffer-size 0)
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (add-to-list 'eglot-stay-out-of 'flymake)
  )
