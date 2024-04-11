;; -*- lexical-binding: t -*-

(defvar fate-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar fate-lisp-dir (expand-file-name "lisp/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash.")

(defvar fate-cache-dir (expand-file-name "cache/" fate-emacs-dir)
  "The root to store runtime data, such as autosave files.")

(defun +fate-load-lisp (file)
  (load (expand-file-name file fate-lisp-dir)))

(+fate-load-lisp "autoloads.el")
(+fate-load-lisp "defaults.el")
(+fate-load-lisp "straight.el")
(+fate-load-lisp "evil.el")

;; packages loaded lazily
(+fate-load-lisp "magit.el")
(+fate-load-lisp "projectile.el")
(+fate-load-lisp "lsp.el")

(+fate-load-lisp "completion.el")
(+fate-load-lisp "editor.el")
(+fate-load-lisp "keybindings.el")
(+fate-load-lisp "ui.el")
(+fate-load-lisp "snippets.el")

(defvar fate-lang-dir (expand-file-name "lang/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash")

(defun +fate-load-lang (file)
  (load (expand-file-name file fate-lang-dir)))

(+fate-load-lang "rust.el")
(+fate-load-lang "python.el")
(+fate-load-lang "cc.el")
(+fate-load-lang "shaders.el")
(+fate-load-lang "elisp.el")
(+fate-load-lang "markdown.el")
(+fate-load-lang "javascript.el") ;; typescript and every other possible flavor
(+fate-load-lang "web.el")
(+fate-load-lang "json.el")
(+fate-load-lang "yaml.el")
(+fate-load-lang "docker.el")

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(safe-local-variable-values '((apheleia-inhibit . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
