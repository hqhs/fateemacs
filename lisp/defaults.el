;; -*- lexical-binding: t -*-
;; better values for emacs out-of-the-box configuration options

(setq confirm-kill-emacs 'y-or-n-p)

;; set PATH from shell
(setq-default exec-path
              (append (split-string (getenv "PATH") path-separator t)
                      (list exec-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; security:

;; Only allow safe local variables
(setq enable-local-variables :safe)
;; Disable local variables in files owned by others
(setq enable-local-eval nil)
;; Ask before following symbolic links to version controlled files
(setq vc-follow-symlinks nil)
;; Confirm before visiting symbolic links to files
(setq find-file-visit-truename t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performance

(unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the default system font) can impact startup time
    ;;   dramatically. The larger the delta, the greater the delay. Even trivial
    ;;   deltas can yield up to a ~1000ms loss, depending also on
    ;;   `window-system' (PGTK builds seem least affected and NS/MAC the most).
    (setq frame-inhibit-implied-resize t)

    ;; PERF: A fair bit of startup time goes into initializing the splash and
    ;;   scratch buffers in the typical Emacs session (b/c they activate a
    ;;   non-trivial major mode, generate the splash buffer, and trigger
    ;;   premature frame redraws by writing to *Messages*). These hacks prevent
    ;;   most of this work from happening for some decent savings in startup
    ;;   time. Our dashboard and `doom/open-scratch-buffer' provide a faster
    ;;   (and more useful) alternative anyway.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)
    ;; PERF,UX: Prevent "For information about GNU Emacs..." line in *Messages*.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves file IO and/or bitmap work (depending on the frame type)
    ;;   that we can no-op for a free 50-100ms saving in startup time.
    (advice-add #'display-startup-screen :override #'ignore))

;; Warn when opening files bigger than 5MB
(setq large-file-warning-threshold (* 5 1024 1024))

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `doom-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat fate-cache-dir "autosave/")
      tramp-auto-save-directory  (concat fate-cache-dir "tramp-autosave/"))

;;; Runtime optimizations

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)

;;; Stricter security defaults
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.

(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (>= libgnutls-version 30605)
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: wemove when we drop 28.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

(use-package autorevert
  :ensure nil ;; built-in
  :init
  (global-auto-revert-mode 1))

(use-package uniquify
  :ensure nil ;; built-in
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :ensure nil ;; built-in
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name "recentf" fate-cache-dir))
  :config
  (recentf-mode 1))

(use-package savehist
  :ensure nil ;; built-in
  :custom
  (savehist-file (expand-file-name "savehist" fate-cache-dir))
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode 1))

(use-package repeat
  :ensure nil ;; built-in
  :custom
  (repeat-mode 1)
  (repeat-exit-key (kbd "RET"))
  (repeat-exit-timeout 2))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (eval-after-load 'evil-collection
    (lambda ()
      (evil-collection-define-key 'normal 'dired-mode-map
        "l" 'dired-find-alternate-file
        "h" (lambda () (interactive) (find-alternate-file ".."))))))
