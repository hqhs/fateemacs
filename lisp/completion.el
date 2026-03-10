;; -*- lexical-binding: t -*-

;; Minibuffer completion: fido-vertical-mode (built-in, no deps)
(fido-vertical-mode 1)
(setq icomplete-prospects-height 15)

;; TAB completes, then indents
(setq tab-always-indent 'complete)

;; In-buffer completion: corfu
(use-package corfu
  :straight t
  :init
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.18
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-quit-at-boundary 'separator)
  :config
  (global-corfu-mode)

  ;; History: remember completion selections across sessions
  (require 'corfu-history)
  (corfu-history-mode 1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; Popup info: show docs for selected candidate
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

;; Cape: composable completion-at-point extensions
;; Adds file completion, dabbrev fallback, and makes LSP completions composable
(use-package cape
  :straight t
  :init
  ;; File path completion in programming buffers
  (add-hook 'prog-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  ;; Dabbrev (dynamic abbreviation) as universal fallback
  (add-hook 'prog-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
  (add-hook 'text-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
  :config
  ;; Make eglot completions non-exclusive so cape sources also contribute
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))
