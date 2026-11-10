;; -*- lexical-binding: t -*-

;; Frame-level tabs: one tab per window configuration, like Vim's `:tabnew'.
;; A tab is a layout, not a file -- nothing here ties tabs to projects or
;; buffers, so they stay hand-managed. The terminal claims one of its own
;; (lisp/term.el); every other tab is the editor.
;;
;; The keys were already there: `tab-bar' is preloaded in Emacs, so the
;; `(when (featurep 'tab-bar) ...)' guards in evil-maps.el fired long before
;; this file ran. That gives `gt'/`gT', `C-w gt'/`C-w gT', `C-w T'
;; (detach window into a tab), and ex `:tabnew' `:tabclose' `:tabonly'
;; `:tabprevious'. Only the mode itself was missing.

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-show 1                 ; hide the bar entirely while there is one tab
        tab-bar-tab-hints t            ; number the tabs; `gt' takes a count
        ;; Keyboard-only, like the rest of this config.
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        ;; Default format prepends history arrows; drop them, keep the tabs.
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :config
  (tab-bar-mode 1))
