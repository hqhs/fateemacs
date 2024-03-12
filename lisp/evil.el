;; -*- lexical-binding: t -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-ex-interactive-search-highlight 'selected-window
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :commands evil-collection-init)

(use-package evil-escape
  :straight t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  :config
  (evil-escape-mode))
