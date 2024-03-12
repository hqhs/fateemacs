;; -*- lexical-binding: t -*-

(use-package magit
  :straight t
  :commands magit-status
  :config
  (evil-collection-init 'magit))
