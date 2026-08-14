;; -*- lexical-binding: t -*-

(use-package magit
  :defer t
  :commands magit-status
  :config
  ;; Keep the magit buffer on screen when visiting a file. By default RET on a
  ;; file/hunk takes over magit's own window, so reviewing the second change
  ;; means navigating back to magit first. Magit already has the command, only
  ;; on the awkward `C-x 4 <return>'.
  (with-eval-after-load 'magit-diff
    (keymap-set magit-diff-section-map "<remap> <magit-visit-thing>"
                #'magit-diff-visit-file-other-window)))

;; In-buffer change indicators: a fringe bar on every line that differs from
;; HEAD, plus hunk-to-hunk navigation, so a file opened from magit shows its
;; own changes without a diff buffer.
(use-package diff-hl
  :ensure nil ;; vendored
  :hook (prog-mode . diff-hl-mode)
  :config
  ;; No autoloads are generated for vendored packages, so the optional
  ;; components have to be required by hand.
  (require 'diff-hl-show-hunk)
  (require 'diff-hl-dired)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  ;; Refresh the marks when magit stages/unstages/commits behind our back.
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Side-by-side diffs. Magit already binds `e' to `magit-ediff-dwim' and `E' to
;; the `magit-ediff' transient in every magit buffer -- these settings just make
;; the result usable.
(use-package ediff
  :ensure nil ;; built-in
  :defer t
  :init
  (setq ;; Control panel in the selected frame. The default spawns a separate
        ;; frame, which on macOS lands behind the Emacs window.
        ediff-window-setup-function #'ediff-setup-windows-plain
        ;; Variants left/right rather than stacked top/bottom.
        ediff-split-window-function #'split-window-horizontally
        ediff-merge-split-window-function #'split-window-horizontally
        ;; Ignore whitespace-only differences by default (-w).
        ediff-diff-options "-w")

  ;; Ediff tears down the window layout it found and does not put it back.
  ;; Stash it on the way in, restore it on the way out.
  (defvar +fate--ediff-window-config nil
    "Window configuration to restore when ediff exits.")

  (defun +fate--ediff-save-window-config ()
    (setq +fate--ediff-window-config (current-window-configuration)))

  (defun +fate--ediff-restore-window-config ()
    (when (window-configuration-p +fate--ediff-window-config)
      (set-window-configuration +fate--ediff-window-config)
      (setq +fate--ediff-window-config nil)))

  (add-hook 'ediff-before-setup-hook #'+fate--ediff-save-window-config)
  (add-hook 'ediff-quit-hook #'+fate--ediff-restore-window-config 100)
  (add-hook 'ediff-suspend-hook #'+fate--ediff-restore-window-config 100))
