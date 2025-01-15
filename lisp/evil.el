;; -*- lexical-binding: t -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil  ; Disable evil-keybindings as evil-collection handles this
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-ex-interactive-search-highlight 'selected-window
        evil-undo-system 'undo-redo
        evil-normal-state-tag   (propertize "[NORMAL]" 'face '(:inherit (success bold)))
        evil-emacs-state-tag    (propertize "[EMACS]" 'face '(:inherit (warning bold)))
        evil-insert-state-tag   (propertize "[INSERT]" 'face '(:inherit (error bold)))
        evil-motion-state-tag   (propertize "[MOTION]" 'face '(:inherit (font-lock-keyword-face bold)))
        evil-visual-state-tag   (propertize "[VISUAL]" 'face '(:inherit (font-lock-constant-face bold)))
        evil-operator-state-tag (propertize "[OPERATOR]" 'face '(:inherit (font-lock-function-name-face bold))))
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-collection-key-blacklist '("SPC")  ; Don't bind leader key
        ;; evil-collection-setup-minibuffer t
        evil-collection-mode-list    ; Explicitly list modes we want to configure
        '(xref
          dired
          magit
          (corfu :defer t)     ; Only load when corfu is loaded
          vertico
          consult
          ;; minibuffer
          compile
          buff-menu
          custom
          (org :after org)     ; Only after org is loaded
          help
          info
          which-key))
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  :config
  (evil-escape-mode))

(use-package evil-easymotion
  :straight t
  :after evil
  :commands (evilem-create evilem-default-keybindings)
  :init
  ;; TODO: document default keybindings here
  ;; w/W -- jump word
  ;; j/k -- jump line up/down
  ;; f/F -- jump to char forward/beckward
  (evilem-default-keybindings "gs"))

(use-package evil-snipe
  :straight t
  :after evil
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
  :after evil
  :config
  (global-evil-surround-mode 1))
