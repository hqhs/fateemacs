;; -*- lexical-binding: t -*-

(defun +fate/sp-c-mode-arrow-condition (id action context)
  "Custom condition to insert -> only after a word in C mode."
  (and (eq action 'insert)
       (save-excursion
         (backward-char 1)
         (looking-back "\\w" 1))))

(defun +fate/sp-c-mode-arrow-post-handler (_id action _context)
  "Move cursor after inserting ->."
  (when (eq action 'insert)
    (forward-char 1)))

(defun +fate/add-clang-format-on-save ()
  (add-hook 'before-save-hook
	    (lambda () (clang-format-buffer))
	    nil
	    ;; buffer local hook
	    t))

(defun +fate/c-mode-common-hook ()
  (progn (eglot-ensure) ;; FIXME(hqhs): fails after update to emacs 30.0
	 (yas-minor-mode-on)
	 (tree-sitter-mode)
	 (tree-sitter-hl-mode)
	 (display-fill-column-indicator-mode)
	 ;; NOTE(hqhs): FOLDING CONFIGURED HERE
	 (outline-minor-mode)
	 (+fate/add-clang-format-on-save)))

(use-package cc-mode
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
  (add-hook 'c-mode-hook '+fate/c-mode-common-hook)
  (add-hook 'c++-mode-hook '+fate/c-mode-common-hook)
  ;;
  ;; Ensure smartparens is loaded before configuring local pairs
  (with-eval-after-load 'smartparens
    (dolist (mode '(c-mode c++-mode))
      (sp-local-pair mode "-" ">"
		     :when '(+fate/sp-c-mode-arrow-condition)
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :actions '(insert)
		     :post-handlers '(+fate/sp-c-mode-arrow-post-handler)))
    )
  )

(use-package clang-format
  :straight t
  :init
  (setq clang-format-style "file"
	clang-format-fallback-style "webkit"))
