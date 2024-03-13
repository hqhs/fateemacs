;; -*- lexical-binding: t -*-

;; (Vertico, Marginalia, Orderless, Consult, Embark).

(use-package vertico
  :straight t
  :init
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  :config
  (vertico-mode)
  (evil-collection-init 'vertico))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  ;; TODO: better configuration for remote editing
  (completion-category-overrides '((file (styles orderless partial-completion)))))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :defer t)

(use-package company
  :straight t
  :commands (company-complete-common)
  :init
  (setq company-minimum-prefix-length 2
	company-tooltip-limit 14
	company-require-match 'never
	company-backends '(company-capf))
  :config
  (global-company-mode))
