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
        evil-undo-system 'undo-redo
        evil-normal-state-tag   (propertize "[NORMAL]" 'face '(:inherit (success bold)))
        evil-emacs-state-tag    (propertize "[EMACS]" 'face '(:inherit (warning bold)))
        evil-insert-state-tag   (propertize "[INSERT]" 'face '(:inherit (error bold)))
        evil-motion-state-tag   (propertize "[MOTION]" 'face '(:inherit (font-lock-keyword-face bold)))
        evil-visual-state-tag   (propertize "[VISUAL]" 'face '(:inherit (font-lock-constant-face bold)))
        evil-operator-state-tag (propertize "[OPERATOR]" 'face '(:inherit (font-lock-function-name-face bold))))
  :config
  (evil-mode 1))

;; `n'/`N' with no search pattern adopt the symbol at point, so that the
;; occurrence highlighting in prog-conf.el and search are one concept rather
;; than two: whatever is lit is what `n' will jump through.
;;
;; NOTE(hqhs): these live here rather than in autoloads.el because
;; `evil-define-motion' is a macro and autoloads.el is loaded before evil.
;; They have to be motions, not plain commands, or `dn'/`cn' stop working.

(defun +fate--search-pattern ()
  "Return the pattern `evil-search-next' would repeat, or nil if there is none."
  (car-safe (if evil-regexp-search regexp-search-ring search-ring)))

(evil-define-motion +fate/search-next (count)
  "Repeat the last search.
With no previous search, search forward for the symbol at point."
  :jump t
  :type exclusive
  (if (+fate--search-pattern)
      (evil-search-next count)
    ;; t = match symbol boundaries, same as the occurrence highlighting.
    (evil-search-word-forward count t)))

(evil-define-motion +fate/search-previous (count)
  "Repeat the last search in the opposite direction.
With no previous search, search backward for the symbol at point."
  :jump t
  :type exclusive
  (if (+fate--search-pattern)
      (evil-search-previous count)
    (evil-search-word-backward count t)))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-key-blacklist '("SPC")  ; Don't bind leader key
        ;; evil-collection-setup-minibuffer t
        evil-collection-mode-list    ; Explicitly list modes we want to configure
        '(corfu
          xref
          dired
          magit
          ediff              ; reached from magit via `e' / `E'
          ibuffer
          compile
          buff-menu
          custom
          occur
          (org :after org)     ; Only after org is loaded
          help
          info
          which-key)
        evil-collection-corfu-key-themes '(default magic-return))
  :config
  (evil-collection-init)
  ;; (evil-set-initial-state 'occur-mode 'normal)
  )

;;; Do-what-I-mean escape
;;
;; Bound via `[remap keyboard-quit]' rather than to `C-g' literally, so it also
;; covers evil's ESC and anything else that reaches `keyboard-quit'.

(defvar +fate-escape-hook nil
  "Hook run by `+fate/escape'.
Each function quits one piece of state and returns non-nil if it did; the first
non-nil return wins and nothing further runs.")

(defun +fate/escape (&optional interactive)
  "Quit whatever state is active, one step at a time, then `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ((run-hook-with-args-until-success '+fate-escape-hook))
          ;; Don't abort macros.
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((unwind-protect (keyboard-quit)
             (when interactive (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'+fate/escape)
(with-eval-after-load 'eldoc (eldoc-add-command '+fate/escape))

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

  ;; Register the evil-state exit as one step of `+fate-escape-hook' rather
  ;; than hard-coding it in the handler.
  (add-hook '+fate-escape-hook
            (lambda ()
              (when (or (evil-insert-state-p) (evil-replace-state-p)
                        (evil-visual-state-p) (evil-operator-state-p))
                (evil-normal-state)
                t))))

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
