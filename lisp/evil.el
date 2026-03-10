;; -*- lexical-binding: t -*-

(use-package evil
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
  :after evil
  :init
  (setq evil-collection-key-blacklist '("SPC")  ; Don't bind leader key
        ;; evil-collection-setup-minibuffer t
        evil-collection-mode-list    ; Explicitly list modes we want to configure
        '(xref
          dired
          magit
          ibuffer
          compile
          buff-menu
          custom
          (org :after org)     ; Only after org is loaded
          help
          info
          which-key))
  :config
  (evil-collection-init)
  ;; (evil-set-initial-state 'occur-mode 'normal)
  )

;; Custom evil-escape replacement (no external dep)
;; "jk" in insert mode → normal mode, with configurable timeout
(defvar +fate-escape-key-sequence "jk")
(defvar +fate-escape-timeout 0.2)
(defvar +fate--escape-timer nil)

(defun +fate--escape-insert-first-key ()
  "Handle first key of escape sequence in insert state."
  (interactive)
  (insert (aref +fate-escape-key-sequence 0))
  (setq +fate--escape-timer
        (run-with-timer
         +fate-escape-timeout nil
         (lambda () (setq +fate--escape-timer nil)))))

(defun +fate--escape-insert-second-key ()
  "Handle second key of escape sequence in insert state."
  (interactive)
  (if +fate--escape-timer
      (progn
        (cancel-timer +fate--escape-timer)
        (setq +fate--escape-timer nil)
        (delete-char -1)
        (evil-normal-state))
    (insert (aref +fate-escape-key-sequence 1))))

(with-eval-after-load 'evil
  (evil-define-key 'insert 'global
    (kbd (string (aref +fate-escape-key-sequence 0))) #'+fate--escape-insert-first-key
    (kbd (string (aref +fate-escape-key-sequence 1))) #'+fate--escape-insert-second-key)

  ;; C-g → escape from evil states, keyboard-quit otherwise
  (global-unset-key (kbd "C-g"))
  (global-set-key (kbd "C-g")
                  (lambda ()
                    (interactive)
                    (cond
                     ((or (evil-insert-state-p)
                          (evil-replace-state-p)
                          (evil-visual-state-p)
                          (evil-operator-state-p))
                      (evil-normal-state))
                     (t
                      (keyboard-quit))))))

(use-package evil-easymotion
  :after evil
  :commands (evilem-create evilem-default-keybindings)
  :init
  ;; TODO: document default keybindings here
  ;; w/W -- jump word
  ;; j/k -- jump line up/down
  ;; f/F -- jump to char forward/beckward
  (evilem-default-keybindings "gs"))

(use-package evil-snipe
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
  :after evil
  :config
  (global-evil-surround-mode 1))
