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

(defun +fate/check-treesit-language (lang)
  "Check if treesit is available and LANG grammar is installed."
  (and (version< "30" emacs-version)
       (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p lang)))

(defun +fate/setup-cc-mode ()
  "Setup proper mode for C/C++ based on treesit availability."
  (when (and (+fate/check-treesit-language 'c)
             (+fate/check-treesit-language 'cpp))
    (setq major-mode-remap-alist
          '((c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)))))

;; Use cc-mode instead of tree-sitter modes
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-or-c++-mode) ;; Use automatic detection for headers
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :init
  ;; Basic indentation settings
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2)
  (setq-default indent-tabs-mode nil)

  ;; enable treesit if external libraries for c/c++ are installed
  (+fate/setup-cc-mode)

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
                                     (innamespace . +)
                                     (arglist-intro . +)
                                     (arglist-cont . c-lineup-gcc-asm-reg)
                                     (arglist-cont-nonempty . c-lineup-arglist)
                                     (arglist-close . c-lineup-close-paren)
                                     (func-decl-cont . +)))))

  (setq c-default-style '((c-mode . "custom-style")
                          (c++-mode . "custom-style"))
        c-syntactic-indentation t)
  :config
  ;; insert '->' after '-' in c/c++
  (dolist (mode '(c-mode-map c++-mode-map))
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow))

  ;; same thing for treesit based modes
  (with-eval-after-load "c-ts-mode"  ; or "c++-ts-mode" - either is sufficient
    (when (and (+fate/check-treesit-language 'c)
               (+fate/check-treesit-language 'cpp))
      (dolist (mode '(c-ts-mode-map c++-ts-mode-map))
        (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow)))))

(use-package clang-format
  :straight t
  :after cc-mode
  :custom
  (clang-format-style "file")
  (clang-format-fallback-style "webkit")
  :config
  (evil-define-key '(normal visual) c-mode-base-map
    (kbd "SPC m f") 'clang-format-buffer))

(use-package ggtags
  :straight t)
