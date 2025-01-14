;; -*- lexical-binding: t -*-

(defvar-local +fate/go-fold-nil-checks t
  "Buffer-local variable to control folding of nil checks in Go files.")

(defun +fate/hs-hide-go-error-checks ()
  "Hide Go error checking patterns."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward  "\\(if err\\s-+.*{\\)" nil t)
      (hs-hide-block))))

(defun +fate/toggle-go-nil-checks ()
  "Toggle folding of nil checks in Go files."
  (interactive)
  (setq-local +fate/go-fold-nil-checks (not +fate/go-fold-nil-checks))
  (if +fate/go-fold-nil-checks
      (progn
        (message "Folding nil checks")
        (+fate/hs-hide-go-error-checks))
    (progn
      (message "Unfolding all")
      (evil-open-folds))))

(defun +fate/go-after-save-hook ()
  "After-save hook for Go files."
  (when +fate/go-fold-nil-checks
    (+fate/hs-hide-go-error-checks)))

(defun +fate/go-mode-hook ()
  ;; Use tabs for indentation
  (setq indent-tabs-mode t)
  (setq tab-width 2) ;; since it's tabs, nobody really cares
  (setq-local company-indent-offset 2)
  (progn
    (eglot-ensure)
    (yas-minor-mode-on)
    (add-hook 'after-save-hook '+fate/go-after-save-hook nil t)
    ))

;; New version using go-ts-mode
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . yas-minor-mode-on))
  :config
  ;; Your existing go-specific functions can stay
  (add-hook 'go-ts-mode-hook '+fate/go-after-save-hook))

(use-package mmm-mode
  :straight t
  :config
  (mmm-add-mode-ext-class 'go-mode nil 'go-sql)
  (mmm-add-classes
   '((go-sql
      :submode sql-mode
      :face mmm-code-submode-face
      :front "\\(\"\\|`\\)[\\n[:space:]]*\\(SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|ALTER\\|DROP\\)"
      :back "\\(\"\\|`\\);")))
  )
