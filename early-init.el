;; -*- lexical-binding: t -*-

;; Suppress lexical-binding warnings for vendored packages
;; that intentionally use dynamic binding (evil-snipe, evil-easymotion)
(setq warning-suppress-log-types '((files lexical-binding)))
(setq warning-suppress-types '((files lexical-binding)))

;; PERF: Defer GC entirely during startup, reset after init
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; PERF: Suspend file-name-handler-alist during startup (speeds up all file I/O)
;; This Emacs.app ships uncompressed .el files, so there is no `jka-compr'
;; handler to preserve for the built-in lisp tree.
(defvar +fate--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ...but `command-line-1' is where Emacs resolves its own file arguments, and
;; `emacs /ssh:host:file' needs the handlers to do that. Restore them for the
;; duration of that one call.
(define-advice command-line-1 (:around (fn args-left) +fate-restore-file-handlers)
  (let ((file-name-handler-alist
         (if args-left
             (copy-sequence +fate--file-name-handler-alist)
           file-name-handler-alist)))
    (funcall fn args-left)))

;; PERF: Suppress redisplay until we're ready
(setq-default inhibit-redisplay t)
(setq-default inhibit-message t)

;; Restore redisplay on first user command (mode-line is set by lisp/modeline.el)
(defun +fate--reset-startup-optimizations-h ()
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (remove-hook 'post-command-hook #'+fate--reset-startup-optimizations-h))
(add-hook 'post-command-hook #'+fate--reset-startup-optimizations-h -100)

;; Restore file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist +fate--file-name-handler-alist)
            ;; Reset GC to reasonable runtime values
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16mb
                  gc-cons-percentage 0.1))
          101)

;;; native-comp hygiene

;; Background compilation of vendored elisp fires on every dependency bump, and
;; the warnings are about other people's code. `--debug-init' still shows them
;; (the argument loop runs before this file is loaded).
(setq native-comp-async-report-warnings-errors init-file-debug
      native-comp-warning-on-missing-source init-file-debug)

;; Some packages check `(featurep 'native-compile)' and then call into the API
;; regardless of whether this build can actually compile anything.
(when (and (featurep 'native-compile)
           (not (native-comp-available-p)))
  (setq features (delq 'native-compile features)))

;; native-comp writes a lot of scratch files; keep them out of /tmp.
(defvar +fate--comp-temp-dir
  (expand-file-name "cache/comp/" user-emacs-directory))

(defun +fate--comp-temp-dir-a (fn &rest args)
  "Run FN with `temporary-file-directory' pointed inside our cache."
  (let ((temporary-file-directory +fate--comp-temp-dir))
    (make-directory temporary-file-directory t)
    (apply fn args)))

(with-eval-after-load 'comp
  ;; Renamed between Emacs versions; advise whichever one exists.
  (dolist (fn '(comp-run-async-workers comp--run-async-workers))
    (when (fboundp fn)
      (advice-add fn :around #'+fate--comp-temp-dir-a))))

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; HACK: Fully disabling the menu bar makes macOS treat Emacs GUI frames as
;;   non-application windows, so they stop capturing input focus on activation
;;   -- the first click into an unfocused frame only raises it instead of
;;   placing point. macOS draws the menu bar outside the frame, so keeping it
;;   costs no screen space. TTY frames draw it in-frame, where it is only
;;   clutter, so they stay at 0.
(push '(menu-bar-lines . 0) default-frame-alist)
(when (eq system-type 'darwin)
  (defun +fate--menu-bar-on-gui-frames-h (&optional frame)
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1)))
  (add-hook 'after-make-frame-functions #'+fate--menu-bar-on-gui-frames-h)
  (add-hook 'window-setup-hook #'+fate--menu-bar-on-gui-frames-h))
