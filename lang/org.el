;; -*- lexical-binding: t -*-

(use-package org
  :config
  (evil-collection-init 'org)
  (add-hook 'org-mode-hook 'org-link-descriptive-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  )
