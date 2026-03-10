;; -*- lexical-binding: t -*-

;; Minibuffer completion: fido-vertical-mode (built-in)
(fido-vertical-mode 1)
(setq icomplete-prospects-height 15)

;; TAB completes, then indents
(setq tab-always-indent 'complete)

;; In-buffer completion popup (corfu)
(use-package corfu
  :ensure nil ;; vendored
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto nil)            ; Manual trigger only (C-SPC)
  (corfu-cycle t)             ; Cycle through candidates
  (corfu-preselect 'prompt)   ; Don't preselect first candidate
  (corfu-quit-no-match t)     ; Quit when no match
  (corfu-count 10))           ; Max candidates shown

;; Custom dabbrev capf (replaces cape-dabbrev)
(require 'dabbrev)

(defun +fate/dabbrev-capf ()
  "Completion-at-point function for dynamic abbreviation."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (dabbrev--reset-global-variables)
      (let ((expansions (dabbrev--find-all-expansions
                         (buffer-substring-no-properties (car bounds) (cdr bounds))
                         nil)))
        (when expansions
          (list (car bounds) (cdr bounds) expansions))))))

(add-hook 'prog-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
(add-hook 'text-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
