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
  (completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))))
