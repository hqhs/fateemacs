;; -*- lexical-binding: t -*-

;; Basic setup

(fido-vertical-mode 1)

;; NOTE(hqhs): fido ("fake ido" uses icomplete settings under the hood)
(setq icomplete-prospects-height 15)

;; Evil integration
(with-eval-after-load 'evil
  (define-key minibuffer-local-map (kbd "C-j") 'next-line)
  (define-key minibuffer-local-map (kbd "C-k") 'previous-line)
  (define-key minibuffer-local-map (kbd "C-n") 'next-line)
  (define-key minibuffer-local-map (kbd "C-p") 'previous-line))

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
