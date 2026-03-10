;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword modes
(when (locate-library "wgsl-mode")
  (use-package wgsl-mode))

(when (locate-library "glsl-mode")
  (use-package glsl-mode))
