;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword modes
(when (locate-library "wgsl-mode")
  (use-package wgsl-mode))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)))

(provide 'shaders)
