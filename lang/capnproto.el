;; -*- lexical-binding: t -*-

(use-package capnp-mode
  :straight (:host github :repo "capnproto/capnproto" :files ("highlighting/emacs/capnp-mode.el"))
  :mode "\\.capnp\\'")
