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

(defun +fate/add-clang-format-on-save ()
  (add-hook 'before-save-hook
            (lambda () (clang-format-buffer))
            nil
            ;; buffer local hook
            t))

;; Use cc-mode instead of tree-sitter modes
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-or-c++-mode) ;; Use automatic detection for headers
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode . yas-minor-mode-on)
         (c++-mode . yas-minor-mode-on))
  :init
  ;; Basic indentation settings
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2)
  (setq-default indent-tabs-mode nil)

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
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow)))

(use-package clang-format
  :straight t
  :after cc-mode
  :hook ((c-mode . (lambda ()
                     (add-hook 'before-save-hook #'clang-format-buffer nil t)))
         (c++-mode . (lambda ()
                      (add-hook 'before-save-hook #'clang-format-buffer nil t))))
  :custom
  (clang-format-style "file")
  (clang-format-fallback-style "webkit")
  :config
  (defun +fate/toggle-clang-format-on-save ()
    "Toggle clang-format-on-save for the current buffer."
    (interactive)
    (if (member #'clang-format-buffer before-save-hook)
        (progn
          (remove-hook 'before-save-hook #'clang-format-buffer t)
          (message "Disabled clang-format-on-save"))
      (add-hook 'before-save-hook #'clang-format-buffer nil t)
      (message "Enabled clang-format-on-save"))))

(use-package ggtags
  :straight t)
