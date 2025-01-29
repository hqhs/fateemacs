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

(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode))
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (c-ts-mode . yas-minor-mode-on)
         (c++-ts-mode . yas-minor-mode-on))
  :init
  ;; Basic indentation settings
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2)  ; Equivalent to tabstop/shiftwidth
  (setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs

  ;; Custom indentation rules equivalent to cinoptions
  (c-add-style "custom-style"
               '((c-basic-offset . 1)
                 (c-offsets-alist . ((case-label . 0)
                                     (statement-case-intro . +)
                                     (access-label . -)
                                     (innamespace . +)
                                     (arglist-intro . +)
                                     (arglist-cont . c-lineup-gcc-asm-reg) ; keep original
                                     (arglist-cont-nonempty . c-lineup-arglist) ; keep original
                                     (arglist-close . c-lineup-close-paren) ; keep original
                                     (func-decl-cont . +)
                                     ))))


  (setq c-default-style '((c-mode . "custom-style")
                          (c++-mode . "custom-style"))
        c-syntactic-indentation t)
  :config
   ;; insert '->' after '-' in c/c++
  (dolist (mode '(c-ts-mode-map c++-ts-mode-map))
    (define-key (symbol-value mode) (kbd "-") '+fate/c-electric-arrow))
  )

(use-package clang-format
  :straight t
  :after c-ts-mode
  :hook ((c-ts-mode . (lambda ()
                        (add-hook 'before-save-hook #'clang-format-buffer nil t)))
         (c++-ts-mode . (lambda ()
                         (add-hook 'before-save-hook #'clang-format-buffer nil t))))
  :custom
  (clang-format-style "file")
  (clang-format-fallback-style "webkit")
  :config
  ;; Optionally, you can define a function to toggle format-on-save
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
  :straight t
  )
