;; -*- lexical-binding: t -*-

;; C/C++ arrow insertion using electric-pair-mode
(defun +fate/c-electric-arrow ()
  "Insert -> if - is typed after a word character."
  (interactive)
  (if (and (eq last-command-event ?-)
           (looking-back "\\(?:\\w\\|]\\)" 1)
           (not (nth 4 (syntax-ppss)))) ; not in comment
      (insert "->")
    (insert "-")))

;; `auto-mode-alist' still names the cc-mode modes; `major-mode-remap-alist'
;; in prog-conf.el redirects them to the treesit modes at file-visit time.
;; The cc-mode style settings below only apply if that remap is removed.
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-or-c++-mode) ;; Use automatic detection for headers
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :init
  ;; Basic indentation settings
  (setq-default tab-width 2
                c-basic-offset 2
                indent-tabs-mode nil
                ;;
                c-syntactic-indentation t
                c-tab-always-indent t
                )

  ;; Hooks for both regular and treesit modes
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'eglot-ensure)
)

  (when (version< "30" emacs-version)
    (dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
      (add-hook hook #'eglot-ensure)
  ))

  ;; Custom indentation rules
  (c-add-style "custom-style"
             '((c-basic-offset . 2)
               (c-offsets-alist . ((case-label . 0)
                                   (statement-case-intro . +)
                                   (access-label . -)
                                   (innamespace . 0)          ; Changed: was -, now 0 for NamespaceIndentation: All
                                   (arglist-intro . +)
                                   (arglist-cont . c-lineup-gcc-asm-reg)
                                   (arglist-cont-nonempty . c-lineup-arglist)
                                   (arglist-close . c-lineup-close-paren)
                                   (func-decl-cont . +)
                                   (substatement-open . 0)    ; Changed: for BreakBeforeBraces: Allman
                                   (class-open . 0)           ; Added: for class braces in Allman style
                                   (namespace-open . 0)       ; Added: for namespace braces
                                   (brace-list-open . 0)      ; Your original setting
                                   (brace-entry-open . 0)     ; Your original setting
                                   (statement-block-intro . +) ; Your original setting
                                   (block-close . 0)          ; Your original setting
                                   (brace-list-close . 0)     ; Your original setting
                                   ))))

  (setq c-default-style '((c-mode . "custom-style")
                          (c++-mode . "custom-style")))
  :config

  ;; insert '->' after '-' in c/c++
  (dolist (mode '(c-mode-map c++-mode-map))
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow))
  )

;; C/C++ run on treesit; cc-mode's `c-offsets-alist' style does not apply.
(use-package c-ts-mode
  :ensure nil ;; built-in
  :init
  ;; Braces on the same line, 2-column bodies -- closest c-ts-mode analogue of
  ;; the "custom-style" above. Live typing only; clang-format owns the file on
  ;; save, so this just needs to not fight it.
  (setq c-ts-mode-indent-style 'k&r)
  :config
  (dolist (mode '(c-ts-mode-map c++-ts-mode-map))
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow)))

;; Format C/C++ via custom formatter (uses clang-format CLI directly)
;; Skip .in template files (e.g. config.hpp.in) — they contain
;; @VAR@/${VAR} placeholders that clang-format mangles.
(dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook
            (lambda ()
              (unless (and buffer-file-name
                           (string-match-p "\\.in\\'" buffer-file-name))
                (setq-local +fate-format-command
                            (list "clang-format"))))))

;; NOTE(hqhs): `indent-bars' guesses indentation width per major mode, and for
;; cc-mode it only accepts `c-basic-offset' when that is buffer-local -- ours is
;; a `setq-default', so the guess falls through to `standard-indent' (4) and the
;; bars land on the wrong columns. State the real offset explicitly instead of
;; depending on which mode is active.
(dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook (lambda () (setq-local indent-bars-spacing-override 2))))

