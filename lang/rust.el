;; -*- lexical-binding: t -*-

;; New version:
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . dtrt-indent-mode)))
