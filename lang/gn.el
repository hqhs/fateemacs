;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword mode
(when (locate-library "gn-mode")
  (use-package gn-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode))))
