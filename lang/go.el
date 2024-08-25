;; -*- lexical-binding: t -*-

(defun +fate/hs-hide-go-error-checks ()
  "Hide Go error checking patterns."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(if err != nil {\\)" nil t)
      (hs-hide-block))))

(defun +fate/go-mode-hook ()
  ;; Use tabs for indentation
  (setq indent-tabs-mode t)
  ;; Set tab width to 4 spaces (Go's default)
  (setq tab-width 2)
  ;; Ensure that go-mode uses tab-width for indentation
  (setq-local indent-line-function 'indent-relative)
  ;; Ensure electric-indent-mode uses tab-width
  (setq-local electric-indent-inhibit t)
  ;; If you're using company-mode for completions
  (setq-local company-indent-offset 2)
  (progn
    (eglot-ensure)
    (tree-sitter-mode)
    (tree-sitter-hl-mode)
    (yas-minor-mode-on)
    (hs-minor-mode)
    ))

(use-package go-mode
  :straight t
  :init
  :config
  (add-hook 'go-mode-hook '+fate/go-mode-hook)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
