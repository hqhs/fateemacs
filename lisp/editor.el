;; -*- lexical-binding: t -*-

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :config
  (require 'tree-sitter-langs))

(use-package editorconfig
  :straight t
  :init
  (editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :straight t
  :config
  ;; TODO: what is smie mode?
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t
        ;; reduced from the default of 5000
        dtrt-indent-max-lines 2000)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

(use-package ws-butler
  :straight t
  :init
    ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.

  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

(use-package pcre2el
  :straight t
  :commands (rxt-quote-pcre))
