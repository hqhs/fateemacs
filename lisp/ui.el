;; -*- lexical-binding: t -*-

(defvar +fate/original-background nil
  "Stores the original background color.")

(defun +fate/toggle-background-transparency ()
  "Toggle between the original background and a transparent background in terminal Emacs."
  (interactive)
  (unless (display-graphic-p)
    ;; wouldn't work in case theme is switched between toggles, but who cares
    (if +fate/original-background
        (progn
          (set-face-background 'default +fate/original-background)
          (setq +fate/original-background nil))
      (setq +fate/original-background (face-background 'default))
      (set-face-background 'default "rgba:0000/0000/0000/7500"))))


;; (add-hook 'after-init-hook 'set-background-for-terminal)

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-github nil
        doom-modeline-minor-modes nil
	doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon nil)
  :config
  (doom-modeline-mode))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-dracula t))

(use-package hl-line
  :config
  (global-hl-line-mode 1))
