;; -*- lexical-binding: t -*-

(defvar fate-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar fate-lisp-dir (expand-file-name "lisp/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash")

(load (expand-file-name "defaults.el" fate-lisp-dir))
(load (expand-file-name "straight.el" fate-lisp-dir))
(load (expand-file-name "evil.el" fate-lisp-dir))

;; packages loaded lazily
(load (expand-file-name "magit.el" fate-lisp-dir))
(load (expand-file-name "projectile.el" fate-lisp-dir))

;;
(load (expand-file-name "completion.el" fate-lisp-dir))
(load (expand-file-name "keybindings.el" fate-lisp-dir))
(load (expand-file-name "ui.el" fate-lisp-dir))

(defvar fate-lang-dir (expand-file-name "lang/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash")

(load (expand-file-name "rust.el" fate-lang-dir))

;; packages I've not yet figured out where to put
(use-package gcmh
  :straight t
  :init
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode 1))

(use-package tree-sitter
  :straight t
  :config
  (require 'tree-sitter))
