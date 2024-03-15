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
  (setq evil-normal-state-tag   (propertize "[NORMAL]" 'face '((:background "green" :foreground "black")))
	evil-emacs-state-tag    (propertize "[EMACS]" 'face '((:background "orange" :foreground "black")))
	evil-insert-state-tag   (propertize "[INSERT]" 'face '((:background "red") :foreground "white"))
	evil-motion-state-tag   (propertize "[MOTION]" 'face '((:background "blue") :foreground "white"))
	evil-visual-state-tag   (propertize "[VISUAL]" 'face '((:background "grey80" :foreground "black")))
	evil-operator-state-tag (propertize "[OPERATOR]" 'face '((:background "purple"))))
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :commands evil-collection-init
  :init
  (add-hook 'compilation-mode-hook (lambda () (evil-collection-init 'compile))))

(use-package evil-escape
  :straight t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  :config
  (evil-escape-mode))

(use-package evil-easymotion
  :straight t
  :commands (evilem-create evilem-default-keybindings)
  :init
  ;; TODO: document default keybindings here
  (evilem-default-keybindings "gs"))

(use-package evil-snipe
  :straight t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-override-mode)
  (evil-snipe-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))
