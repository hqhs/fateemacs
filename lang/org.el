;; -*- lexical-binding: t -*-

(use-package org
  :config
  ;; NOTE(hqhs): doesn't work in emacs 30
  ;; (add-hook 'org-mode-hook 'org-link-descriptive-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  )
