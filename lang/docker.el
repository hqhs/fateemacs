;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :straight t
  :commands dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )
