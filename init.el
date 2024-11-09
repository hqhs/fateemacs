;; -*- lexical-binding: t -*-

;;

(defvar fate-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar fate-lisp-dir (expand-file-name "lisp/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash.")

(defvar fate-cache-dir (expand-file-name "cache/" fate-emacs-dir)
  "The root to store runtime data, such as autosave files.")

(defun +fate-load-lisp (file)
  (load (expand-file-name file fate-lisp-dir)))

(+fate-load-lisp "straight.el")
;; requiring it before straight is loaded breaks clean installation
(use-package cl-lib) ;; TODO(hqhs): where should it live?..

(+fate-load-lisp "autoloads.el")
(+fate-load-lisp "defaults.el")
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
(+fate-load-lang "meson.el")
(+fate-load-lang "go.el")
(+fate-load-lang "mdesk.el")
(+fate-load-lang "shaders.el")
(+fate-load-lang "org.el")
(+fate-load-lang "elisp.el")
(+fate-load-lang "markdown.el")
(+fate-load-lang "javascript.el") ;; typescript and every other possible flavor
(+fate-load-lang "gdscript.el")
(+fate-load-lang "web.el")
(+fate-load-lang "json.el")
(+fate-load-lang "yaml.el")
(+fate-load-lang "docker.el")
(+fate-load-lang "cmake.el")

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
   '("b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "scons --gpu=wgpu-native --editor pizza && ./build/pizza")
     (projectile-project-compilation-cmd . "scons --gpu=wgpu-native --editor thegame && ./build/thegame")
     (eglot-connect-timeout . 120)
     (eglot-server-programs
      (c-mode "clangd" "--background-index" "--clang-tidy" "--header-insertion=never" "--completion-style=detailed" "--function-arg-placeholders" "--fallback-style=llvm")
      (c++-mode "clangd" "--background-index" "--clang-tidy" "--header-insertion=never" "--completion-style=detailed" "--function-arg-placeholders" "--fallback-style=llvm"))
     (eglot-ignored-server-capabilities . t)
     (apheleia-inhibit . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
