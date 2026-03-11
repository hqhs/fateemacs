;; -*- lexical-binding: t -*-

;; Use built-in js-json-mode (font-lock, no tree-sitter dependency)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-json-mode))

(provide 'json)
