;; -*- lexical-binding: t -*-

;; Terminal: ghostel, which drives Ghostty's terminal engine (libghostty-vt)
;; through an Emacs dynamic module. The engine does the VT parsing and screen
;; state; Emacs still renders, so this is a terminal in a buffer rather than a
;; second renderer.
;;
;; Unlike everything else in vendor/, half of ghostel is Zig and has to be
;; compiled: ./scripts/build-term.sh (needs zig, see README). That is
;; deliberately optional. Nothing here is `require'd at startup, so a clone
;; with no module built starts normally and `+fate/term' says what to run
;; rather than erroring. `ghostel.el' calls `ghostel--load-module' at load
;; time, which is why this file must not `require' it eagerly.

(defvar +fate-ghostel-dir (expand-file-name "ghostel/" fate-vendor-dir)
  "Directory holding the vendored ghostel elisp and its compiled module.")

(defvar +fate-ghostel-module
  (expand-file-name (concat "ghostel-module" (or module-file-suffix ".so"))
                    +fate-ghostel-dir)
  "Path to the native module that `scripts/build-term.sh' produces.")

(defun +fate/term-available-p ()
  "Return non-nil when the ghostel native module is built and loadable."
  (and module-file-suffix                ; Emacs built --with-modules
       (file-exists-p +fate-ghostel-module)))

(use-package ghostel
  :defer t
  :init
  ;; Never phone home. The default (`ask') offers to download a prebuilt
  ;; binary on first use, which is exactly what the vendoring rules forbid --
  ;; the module comes from build-term.sh or not at all.
  (setq ghostel-module-auto-install nil
        ghostel-module-directory +fate-ghostel-dir)
  :config
  ;; Evil integration, modeled on evil-collection-vterm: operators clamp to
  ;; the shell's input region and apply over the PTY, and full-screen TUIs
  ;; fall through to the application instead of being swallowed by Evil.
  ;; No autoloads are generated for vendored packages, so require it by hand.
  (with-eval-after-load 'evil
    (require 'evil-ghostel)))

(defun +fate/term ()
  "Open a ghostel terminal buffer.
Reports what to build when the native module is missing, instead of
failing inside `module-load'."
  (interactive)
  (unless (+fate/term-available-p)
    (user-error
     "No terminal module: run ./scripts/build-term.sh (needs zig)"))
  (require 'ghostel)
  (call-interactively #'ghostel))
