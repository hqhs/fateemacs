;; -*- lexical-binding: t -*-

(use-package eldoc
  ;; used by eglot
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :straight t
  :commands eglot-ensure)
