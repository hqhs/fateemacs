;; -*- lexical-binding: t -*-

(use-package web-mode
  :straight t
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode)))
