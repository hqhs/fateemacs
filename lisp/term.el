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
  ;; No autoloads are generated for vendored packages, so require by hand.
  (with-eval-after-load 'evil
    (require 'evil-ghostel)
    (+fate--term-setup-evil)))

(defun +fate--term-setup-evil ()
  "Wire evil-ghostel into ghostel buffers.
Split out of the `ghostel' :config block so it can be tested without
loading the terminal stack."
  ;; Evil integration, modeled on evil-collection-vterm: operators clamp to
  ;; the shell's input region and apply over the PTY, and full-screen TUIs
  ;; fall through to the application instead of being swallowed by Evil.
  ;; `evil-ghostel-mode' is a plain buffer-local minor mode with no
  ;; self-enabling hook, so requiring the file alone leaves it all inert.
  (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)

  ;; The `jk' escape in lisp/evil.el binds j and k in *every* insert state,
  ;; and its handler reaches for `insert' -- which in a ghostel buffer writes
  ;; into renderer-owned text instead of the shell, so plain `j' and `k' never
  ;; arrive at the terminal. Evil's state maps outrank the major mode's, so
  ;; the only way to get those two keys back is to outrank evil in turn: an
  ;; auxiliary binding on evil-ghostel's own minor-mode map. Binding to
  ;; `self-insert-command' rather than to ghostel's private sender keeps them
  ;; going through ghostel's `[remap self-insert-command]', like every other
  ;; printable key.
  (evil-define-key* 'insert evil-ghostel-mode-map
                    (kbd "j") #'self-insert-command
                    (kbd "k") #'self-insert-command))

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
