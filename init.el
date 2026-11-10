;; -*- lexical-binding: t -*-

;; startup window size

(setq initial-frame-alist
      '((width . 120)
        (height . 45)))

;; NOTE(hqhs): `append', not a bare `setq'. A plain assignment here discarded
;; everything early-init.el pushes onto this alist -- the menu-bar, tool-bar
;; and scroll-bar suppression included -- which is why those pushes had no
;; effect. The macOS menu-bar hack in early-init.el depends on this.
(setq default-frame-alist
      (append '((width . 120)
                (height . 45)
                (left . 100)   ; pixels from left
                (top . 50))    ; pixels from top
              default-frame-alist))

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
            docstrings
            lexical-binding))

;; disallow some packages from loading completely

(setq package-load-list '(all))

(setq flycheck-mode nil)
(with-eval-after-load 'flycheck
  (global-flycheck-mode -1))
(push '(flycheck nil) package-load-list)


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

;; NOTE(hqhs): `auto-save-file-name-transforms' is set in lisp/defaults.el,
;; next to `auto-save-list-file-prefix' and the directory that gets created for
;; it. It used to be set here, pointing at a "auto-save/" directory nothing ever
;; created, which is why auto-save silently did nothing.

(defun +fate-load-lisp (file)
  (load (expand-file-name file fate-lisp-dir)))

(+fate-load-lisp "straight.el") ;; vendor loader + use-package
(require 'cl-lib)

;;; Utility macros (custom lisp, no deps)

(defmacro +fate/add-transient-hook! (hook-or-function &rest forms)
  "Attach FORMS to HOOK-OR-FUNCTION. Runs once, then removes itself.
If HOOK-OR-FUNCTION is a hook symbol, adds to hook. If a function symbol, advises it."
  (declare (indent 1))
  (let ((fn (gensym "+fate-transient-")))
    `(progn
       (fset ',fn (lambda (&rest _)
                    ,@forms
                    (if (functionp ,hook-or-function)
                        (advice-remove ,hook-or-function #',fn)
                      (remove-hook ,hook-or-function #',fn))
                    (fmakunbound ',fn)))
       (if (functionp ,hook-or-function)
           (advice-add ,hook-or-function :before #',fn)
         (add-hook ,hook-or-function #',fn)))))

(defmacro +fate/with-delayed-gc! (&rest body)
  "Run BODY with the garbage collector effectively switched off.
For wrapping expensive hook bodies: a collection in the middle of one is pure
latency, and the allocation is short-lived anyway."
  (declare (indent 0))
  `(let ((gc-cons-threshold most-positive-fixnum)
         (gc-cons-percentage 1.0))
     ,@body))

;;; Startup deferral
;;
;; Two hooks chained off real triggers, so a mode that is only needed once the
;; session is interactive doesn't cost anything at startup.

(defvar +fate-first-input-hook nil
  "Hook run before the first user command, then reset to nil.")

(defvar +fate-first-file-hook nil
  "Hook run before the first interactively opened file, then reset to nil.")

(defun +fate-run-hook-on (hook-var trigger-hooks)
  "Run HOOK-VAR once, the first time any of TRIGGER-HOOKS fires post-startup.
HOOK-VAR is reset to nil afterwards so its functions can be GC'd."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "+fate-chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset fn
            (lambda (&rest _)
              ;; `after-init-time' is set before `command-line-1' opens file
              ;; arguments, so `emacs foo.c' still triggers first-file.
              (when (and (not running?) after-init-time
                         (boundp hook) (symbol-value hook))
                ;; The hooks could change the current buffer, which breaks
                ;; `after-find-file' for our caller.
                (save-current-buffer
                  (setq running? t)
                  (run-hooks hook-var)
                  (set hook-var nil)))))
      ;; Hooks are sometimes lexically unset to suppress them during batch
      ;; buffer processing; the `boundp'/`symbol-value' guard above treats that
      ;; as non-interactive and declines to fire.
      (add-hook hook fn -101))))

(+fate-run-hook-on '+fate-first-input-hook '(pre-command-hook))
(+fate-run-hook-on '+fate-first-file-hook  '(find-file-hook dired-initial-position-hook))

;; Large file protection: auto-enable so-long handling (built-in) for huge
;; files. Deliberately NOT deferred to `+fate-first-file-hook' the way the
;; rest of Phase 2 is: the first file opened in a session is exactly the one
;; that could be a 40MB minified blob, and loading so-long.el costs a few ms.
;; Configured in lisp/defaults.el.
(global-so-long-mode 1)

(+fate-load-lisp "monokai-theme.el")
(+fate-load-lisp "autoloads.el")
(+fate-load-lisp "defaults.el")
(+fate-load-lisp "evil.el")

;; packages loaded lazily
(+fate-load-lisp "magit.el")
(+fate-load-lisp "project.el") ;; built-in and lighter alternative to projectile
(+fate-load-lisp "lsp.el")
(+fate-load-lisp "tags.el")
(+fate-load-lisp "jumps.el") ;; after tags.el/project.el: it advises their commands
(+fate-load-lisp "completion.el")
(+fate-load-lisp "prog-conf.el") ;; prog-mode is "base" configuration for toher major modes like rust, cc, go, etc.
(+fate-load-lisp "keybindings.el")
(+fate-load-lisp "modeline.el")
(+fate-load-lisp "ui.el")
(+fate-load-lisp "snippets.el")
(+fate-load-lisp "tabs.el")  ;; tab-bar; term.el puts the terminal in a tab
(+fate-load-lisp "term.el")  ;; ghostel; no-op until scripts/build-term.sh runs

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
(+fate-load-lang "meson.el")
(+fate-load-lang "mdesk.el")
(+fate-load-lang "shaders.el")
(+fate-load-lang "org.el")
(+fate-load-lang "elisp.el")
(+fate-load-lang "markdown.el")
(+fate-load-lang "javascript.el") ;; typescript and every other possible flavor
(+fate-load-lang "flatbuffers.el")
(+fate-load-lang "capnproto.el")
(+fate-load-lang "web.el")
(+fate-load-lang "json.el")
(+fate-load-lang "yaml.el")
(+fate-load-lang "docker.el")
(+fate-load-lang "cmake.el")
(+fate-load-lang "mlir.el")

;; Emacs writes to `custom-file' whenever something calls
;; `customize-save-variable' (`load-theme's safe-theme prompt, answering `!' at
;; a dir-locals prompt). With `custom-file' nil that target is this file, which
;; is how a 40-line `custom-set-variables' block ended up here. It is pointed
;; at cache/custom.el in lisp/defaults.el; the values worth keeping were moved
;; into `safe-local-variable-values' there, by hand.
(load custom-file t t)
