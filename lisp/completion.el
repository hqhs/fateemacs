;; -*- lexical-binding: t -*-

;; Basic setup

(fido-vertical-mode 1)

;; NOTE(hqhs): fido ("fake ido" uses icomplete settings under the hood)
(setq icomplete-prospects-height 15)

(use-package consult
  :straight t
  :defer t
  :init
  (setq consult-narrow-key "<"))

(use-package company
  :straight t
  :commands (company-complete-common)
  :init
  (setq company-minimum-prefix-length 2
	company-tooltip-limit 14
	company-require-match 'never
	company-backends '(company-capf))
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-w") nil t))
