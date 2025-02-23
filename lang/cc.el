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

;; Use cc-mode instead of tree-sitter modes
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
    (add-hook hook #'yas-minor-mode-on))

  (when (version< "30" emacs-version)
    (dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
      (add-hook hook #'eglot-ensure)
      (add-hook hook #'yas-minor-mode-on)))

  ;; Custom indentation rules
  (c-add-style "custom-style"
               '((c-basic-offset . 1)
                 (c-offsets-alist . ((case-label . 0)
                                     (statement-case-intro . +)
                                     (access-label . -)
                                     (innamespace . -)
                                     (arglist-intro . +)
                                     (arglist-cont . c-lineup-gcc-asm-reg)
                                     (arglist-cont-nonempty . c-lineup-arglist)
                                     (arglist-close . c-lineup-close-paren)
                                     (func-decl-cont . +)
                                     (block-open . 0)          ; Controls opening braces for blocks
                                     (brace-list-open . 0)     ; Controls opening braces for lists
                                     (brace-entry-open . 0)    ; Controls opening braces for entries
                                     (statement-block-intro . +) ; Controls indentation after an opening brace
                                     (block-close . 0)         ; Controls closing braces
                                     (brace-list-close . 0)    ; Controls closing braces for lists
                                     ))))

  (setq c-default-style '((c-mode . "custom-style")
                          (c++-mode . "custom-style")))
  :config

  ;; insert '->' after '-' in c/c++
  (dolist (mode '(c-mode-map c++-mode-map))
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow))
  )

(use-package clang-format
  :straight t
  :after cc-mode
  :custom
  (clang-format-style "file")
  (clang-format-fallback-style "webkit")
  :config
  (evil-define-key '(normal visual) c-mode-base-map
    (kbd "SPC m f") 'clang-format-buffer)
  )

(use-package ggtags
  :straight t)
