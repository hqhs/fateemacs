;; -*- lexical-binding: t -*-

(define-derived-mode go-template-mode prog-mode "GoTmpl"
  (setq font-lock-defaults
        '((("\\$[a-zA-Z0-9_]*" . font-lock-variable-name-face)
           ("\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)" . (1 font-lock-variable-name-face))
           ("\\b\\(define\\|else\\|end\\|if\\|range\\|template\\|with\\|block\\)\\b" . font-lock-keyword-face)
           ("\\b\\(and\\|or\\|not\\|len\\|index\\|print\\|printf\\|println\\)\\b" . font-lock-builtin-face))
          nil nil)))

(use-package polymode
  :straight t
  :config
  ;; go with sql queries
  (define-hostmode poly-go-hostmode :mode 'go-ts-mode)
  (define-innermode poly-sql-innermode
    :mode 'sql-mode
    :head-matcher "\\(\"\\|`\\)[\\n[:space:]]*\\(SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|ALTER\\|DROP\\)"
    :tail-matcher "\\(\"\\|`\\);"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-go-sql-mode
    :hostmode 'poly-go-hostmode
    :innermodes '(poly-sql-innermode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . poly-go-sql-mode))
  ;; go templates with c++
  (define-hostmode poly-c++-hostmode :mode 'c++-ts-mode)
  (define-innermode poly-go-template-innermode
    :mode 'go-template-mode
    :head-matcher "{{"
    :tail-matcher "}}"
    :head-mode 'body
    :tail-mode 'body)
  (define-polymode poly-c++-go-template-mode
    :hostmode 'poly-c++-hostmode
    :innermodes '(poly-go-template-innermode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\.in\\'" . poly-c++-go-template-mode)))
