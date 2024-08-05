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
  :straight t
  :init
  (setq eglot-autoshutdown t

	;; to view capabilities:
	;; (kill-new (format "%S" (eglot--capabilities (eglot--current-server-or-lose))))
	eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)
        ;; NOTE This setting disable the eglot-events-buffer enabling more
        ;;      consistent performance on long running emacs instance.
        ;;      Default is 2000000 lines. After each new event the whole buffer
        ;;      is pretty printed which causes steady performance decrease over time.
        ;;      CPU is spent on pretty priting and Emacs GC is put under high pressure
        eglot-events-buffer-size 0)

  :config
  ;; replace
  (setq eglot-server-programs
	(cons '(c++-mode . ("clangd" "--header-insertion=never"))
	      (cl-remove-if (lambda (entry)
			      (and (listp (car entry))
				   (member 'c++-mode (car entry))))
			    eglot-server-programs)))
  ;; add
  (push '(c-mode . ("clangd" "--header-insertion=never"))
	eglot-server-programs)

  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (add-hook 'eglot-managed-mode-hook
   (lambda ()
     ;; we want eglot to setup callbacks from eldoc, but we don't want eldoc
     ;; running after every command. As a workaround, we disable it after we just
     ;; enabled it. Now calling `M-x eldoc` will put the help we want in the eldoc
     ;; buffer. Alternatively we could tell eglot to stay out of eldoc, and add
     ;; the hooks manually, but that seems fragile to updates in eglot.
     (eldoc-mode -1)))
  (add-to-list 'eglot-stay-out-of 'flymake))
