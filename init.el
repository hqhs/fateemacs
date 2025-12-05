;; -*- lexical-binding: t -*-

;; startup window size

(setq initial-frame-alist
      '((width . 120)
        (height . 45)))

(setq default-frame-alist
      '((width . 120)
        (height . 45)
        (left . 100)    ; pixels from left
        (top . 50)))   ; pixels from top

;; disable some warnings

(setq byte-compile-warnings
      '(not free-vars
            unresolved
            callargs
            redefine
            obsolete
            noruntime
            cl-functions
            interactive-only
            make-local
            mapcar
            docstrings))

;; disallow some packages from loading completely

(setq package-load-list '(all))

(setq flycheck-mode nil)
(with-eval-after-load 'flycheck
  (global-flycheck-mode -1))
(push '(flycheck nil) package-load-list)

(setq flymake-mode nil)
(with-eval-after-load 'flymake
  (flymake-mode -1))
(push '(flymake nil) package-load-list)

;; enhance security

(setq network-security-level 'high
      gnutls-verify-error t
      gnutls-min-prime-bits 3072
      password-cache-expiry 3600 ;; 1 hour instead of 16 seconds
      )

(defvar fate-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar fate-lisp-dir (expand-file-name "lisp/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash.")

(defvar fate-cache-dir (expand-file-name "cache/" fate-emacs-dir)
  "The root to store runtime data, such as autosave files.")

(setq auto-save-file-name-transforms
      `((".*" ,(concat fate-cache-dir "auto-save/") t)))

(defun +fate-load-lisp (file)
  (load (expand-file-name file fate-lisp-dir)))

(+fate-load-lisp "straight.el")
;; requiring it before straight is loaded breaks clean installation
(use-package cl-lib ;; TODO(hqhs): where should it live?..
  :ensure nil ;; built-in
  )

(+fate-load-lisp "monokai-theme.el")
(+fate-load-lisp "autoloads.el")
(+fate-load-lisp "defaults.el")
(+fate-load-lisp "evil.el")

;; packages loaded lazily
(+fate-load-lisp "magit.el")
(+fate-load-lisp "project.el") ;; built-in and lighter alternative to projectile
(+fate-load-lisp "lsp.el")
(+fate-load-lisp "completion.el")
(+fate-load-lisp "prog-conf.el") ;; prog-mode is "base" configuration for toher major modes like rust, cc, go, etc.
(+fate-load-lisp "keybindings.el")
(+fate-load-lisp "ui.el")
(+fate-load-lisp "snippets.el")

;; After loading treesit config
(with-eval-after-load 'treesit
  (+fate/ensure-treesit-languages))

(defvar fate-lang-dir (expand-file-name "lang/" fate-emacs-dir)
  "the root directory of lisp files. Must end with a slash")

(defun +fate-load-lang (file)
  (load (expand-file-name file fate-lang-dir)))

(+fate-load-lang "rust.el")
(+fate-load-lang "python.el")
(+fate-load-lang "gn.el")
(+fate-load-lang "cc.el")
(+fate-load-lang "go.el")
(+fate-load-lang "polymode.el") ;; multiple major mode
(+fate-load-lang "meson.el")
(+fate-load-lang "mdesk.el")
(+fate-load-lang "shaders.el")
(+fate-load-lang "org.el")
(+fate-load-lang "elisp.el")
(+fate-load-lang "markdown.el")
(+fate-load-lang "javascript.el") ;; typescript and every other possible flavor
(+fate-load-lang "flatbuffers.el")
(+fate-load-lang "capnproto.el")
(+fate-load-lang "gdscript.el")
(+fate-load-lang "web.el")
(+fate-load-lang "json.el")
(+fate-load-lang "yaml.el")
(+fate-load-lang "docker.el")
(+fate-load-lang "cmake.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da"
     "603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961"
     "2918f362c1418488daa9ea3beaa32df0cb928f2be5a746b09535320fae96badc"
     "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f"
     "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443"
     "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577"
     "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a"
     "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006"
     "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     default))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd
      . "scons --gpu=wgpu-native --editor pizza && ./build/pizza")
     (projectile-project-compilation-cmd
      . "scons --gpu=wgpu-native --editor thegame && ./build/thegame")
     (eglot-connect-timeout . 120)
     (eglot-server-programs
      (c-mode "clangd" "--background-index" "--clang-tidy"
              "--header-insertion=never" "--completion-style=detailed"
              "--function-arg-placeholders" "--fallback-style=llvm")
      (c++-mode "clangd" "--background-index" "--clang-tidy"
                "--header-insertion=never"
                "--completion-style=detailed"
                "--function-arg-placeholders" "--fallback-style=llvm"))
     (eglot-ignored-server-capabilities . t) (apheleia-inhibit . t))))
