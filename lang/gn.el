;; -*- lexical-binding: t -*-

(use-package gn-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode)))
