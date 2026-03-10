;; -*- lexical-binding: t -*-

;; Custom mode-line: show only what matters.
;; No minor modes, no mode-line-modes construct.

(setq-default mode-line-format
              '((:eval evil-mode-line-tag)
                "%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                (:eval (format-mode-line "%l:%c"))
                (vc-mode vc-mode)
                "  "
                (:propertize mode-name)
                mode-line-end-spaces))
