;; -*- lexical-binding: t -*-

;; Minibuffer completion: fido-vertical-mode (built-in). Deferred to the first
;; command -- `pre-command-hook' fires before the command that opens a
;; minibuffer gets there.
(add-hook '+fate-first-input-hook #'fido-vertical-mode)
(setq icomplete-prospects-height 15)

;; C-j/C-k to navigate candidates in minibuffer
(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-j") #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-k") #'icomplete-backward-completions))

;; TAB completes, then indents
(setq tab-always-indent 'complete)

;; In-buffer completion popup (corfu)
(use-package corfu
  :ensure nil ;; vendored
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preview-current t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-count 10))

(defun +fate/toggle-corfu-auto ()
  "Toggle automatic completion popup globally."
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (message "Corfu auto-completion %s" (if corfu-auto "enabled" "disabled")))

