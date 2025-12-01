;; -*- lexical-binding: t -*-

;; Basic setup

(fido-vertical-mode 1)

;; NOTE(hqhs): fido ("fake ido" uses icomplete settings under the hood)
(setq icomplete-prospects-height 15)

(use-package corfu
  :straight t
  :init
  (setq corfu-auto t
        corfu-auto-prefix 2)
  :config
  (global-corfu-mode))
