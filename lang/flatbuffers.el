;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword mode
(when (locate-library "flatbuffers-mode")
  (use-package flatbuffers-mode
    :mode "\\.fbs\\'"))
