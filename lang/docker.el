;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword mode
(when (locate-library "dockerfile-mode")
  (use-package dockerfile-mode
    :commands dockerfile-mode
    :init
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))
