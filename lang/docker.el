;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :commands dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )
