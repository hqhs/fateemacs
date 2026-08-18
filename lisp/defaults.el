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
;; Disable enriched text mode (historical code execution vector via .rtf/.enriched)
(setq enriched-mode nil)
(with-eval-after-load 'enriched
  (defun enriched-decode-display-prop (_start _end &optional _param) nil))
(setq auto-mode-alist (assoc-delete-all "\\.rtf\\'" auto-mode-alist #'string=))
;; Disable inline image rendering (attack surface via libpng/libjpeg/etc.)
(setq auto-image-file-mode nil)
(setq inhibit-images t)

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

(make-directory (concat fate-cache-dir "autosave/") t)
(make-directory (concat fate-cache-dir "tramp-autosave/") t)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `fate-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat fate-cache-dir "autosave/")
      tramp-auto-save-directory  (concat fate-cache-dir "tramp-autosave/")
      ;; Emacs builds auto-save names by concatenating the prefix with the full
      ;; buffer path, which overruns filesystem name limits on deep trees. `sha1'
      ;; compresses that to ~40 characters. The TRAMP rule comes first because
      ;; the default one would write remote auto-saves into
      ;; `temporary-file-directory', which TRAMP prompts about every single time.
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat (concat fate-cache-dir "autosave/") "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ,(file-name-concat (concat fate-cache-dir "autosave/") "\\2-") sha1)))

;; The transforms above are useless if the directory is missing, which is the
;; bug that made auto-save look broken. #o700: auto-saves hold unsaved work.
(add-hook 'auto-save-hook
          (lambda ()
            (with-file-modes #o700
              (make-directory auto-save-list-file-prefix t))))

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

;; PERF: Don't auto-adjust window height for tall lines
(setq auto-window-vscroll nil)

;; PERF: Horizontal scrolling tuning
(setq hscroll-margin 2
      hscroll-step 1)

;; PERF: Blinking cursor causes needless redraws and can freeze on macOS
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; PERF: Don't stretch cursor for wide chars (tabs etc.)
(setq x-stretch-cursor nil)

;; PERF: PGTK-specific latency reduction
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; 1MB for LSP responsiveness.
(setq read-process-output-max (* 1024 1024))  ; 1mb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Menu/tool/scroll bars are already disabled in early-init.el via
;; default-frame-alist. Set mode vars to nil so toggling works correctly.
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

;; NOTE(hqhs): `gnutls-verify-error' is deliberately absent here. init.el sets
;; it to t; Doom sets it to `noninteractive' (i.e. nil in an interactive
;; session) to avoid breaking package installs, and since defaults.el loads
;; after init.el that value used to silently win. `tls-checktrust' below picks
;; up init.el's setting.
(setq gnutls-algorithm-priority
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
;; By default SPC answers "yes" at a `y-or-n-p' prompt. With SPC as the leader
;; key that is far too easy to hit by accident.
(define-key y-or-n-p-map " " nil)

;; The stock undo limits are small enough that a long editing session in a large
;; file silently truncates its own history. Doom's numbers (emacs/undo module).
(setq undo-limit 256000          ; 256kb (default 160kb)
      undo-strong-limit 2000000  ; 2mb   (default 240kb)
      undo-outer-limit 36000000) ; 36mb  (default 24mb)

;; `global-auto-revert-mode' either burns a file watcher per buffer or polls the
;; entire buffer list every `auto-revert-interval' seconds; both degrade as the
;; buffer list grows into the hundreds. Doom's answer is to revert lazily
;; instead: only buffers that are actually on screen, and only at the moments
;; something could have changed underneath them. Doom needs custom switch-buffer
;; hooks for this; Emacs 27+ gives us the same signals for free.
(use-package autorevert
  :ensure nil ;; built-in
  :demand t
  :custom
  (auto-revert-verbose t)             ; let us know when it happens
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  (revert-without-query (list "."))   ; only prompt when the buffer is unsaved
  :config
  (defun +fate--auto-revert-buffer-h ()
    "Revert the current buffer if the file changed underneath it."
    (unless (or auto-revert-mode
                (active-minibuffer-window)
                (and buffer-file-name
                     auto-revert-remote-files
                     (file-remote-p buffer-file-name nil t)))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +fate--auto-revert-visible-buffers-h (&rest _)
    "Revert every buffer currently displayed in a window."
    (dolist (win (window-list nil 'no-minibuffer))
      (with-current-buffer (window-buffer win)
        (+fate--auto-revert-buffer-h))))

  ;; Buffer shown in a window / window selected / anything saved / frame
  ;; refocused after using another app.
  (add-hook 'window-buffer-change-functions #'+fate--auto-revert-visible-buffers-h)
  (add-hook 'window-selection-change-functions #'+fate--auto-revert-visible-buffers-h)
  (add-hook 'after-save-hook #'+fate--auto-revert-visible-buffers-h)
  (add-function :after after-focus-change-function
                #'+fate--auto-revert-visible-buffers-h))

(use-package saveplace
  :ensure nil ;; built-in
  :custom
  (save-place-file (expand-file-name "saveplace" fate-cache-dir))
  :config
  (save-place-mode 1)

  ;; Restoring point near the bottom of a file leaves it pinned to the last
  ;; screen line otherwise.
  (advice-add 'save-place-find-file-hook :after-while
              (lambda (&rest _)
                (if buffer-file-name (ignore-errors (recenter)))))

  ;; If something else already moved point (a jump straight into a location),
  ;; it knows better than the cache does.
  (advice-add 'save-place-find-file-hook :before-while
              (lambda (&rest _) (bobp)))

  ;; `save-place-alist-to-file' runs the whole alist through `pp', which is slow
  ;; for long lists and pointless for a cache file.
  (advice-add 'save-place-alist-to-file :around
              (lambda (fn &rest args)
                (cl-letf (((symbol-function 'pp) #'prin1))
                  (apply fn args)))))

(use-package which-key
  :ensure nil ;; built-in as of Emacs 30
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.1)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil) ; jarring to separate keys by case
  (which-key-add-column-padding 1)     ; less packed UI
  (which-key-min-display-lines 7)      ; prevent a short+wide which-key pane
  (which-key-side-window-slot -10)     ; don't replace popups
  (which-key-compute-remaps t)         ; show remapped commands
  (which-key-ellipsis "…")
  :config
  (which-key-mode 1))

(use-package uniquify
  :ensure nil ;; built-in
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :ensure nil ;; built-in
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name "recentf" fate-cache-dir))
  ;; Cleaning up on a timer stats every remembered file while you are working.
  ;; Quitting is the one moment when that latency costs nothing.
  (recentf-auto-cleanup 'never)
  :config
  ;; Text properties inflate the save file for no benefit. Must come first in
  ;; `recentf-filename-handlers' -- `add-to-list' prepends.
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  ;; Negative depth so the cleanup runs before `recentf-save-list', which
  ;; `recentf-mode' puts on `kill-emacs-hook' at the default depth.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -50)
  (recentf-mode 1))

(use-package savehist
  :ensure nil ;; built-in
  :custom
  (savehist-file (expand-file-name "savehist" fate-cache-dir))
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval nil) ; save on kill only
  (savehist-additional-variables
   '(kill-ring                       ; persist clipboard
     register-alist                  ; persist macros
     mark-ring global-mark-ring      ; persist marks
     search-ring regexp-search-ring)); persist searches
  :config
  (add-hook 'savehist-save-hook
            (lambda ()
              "Strip text properties from `kill-ring' and `register-alist'.
They are the bulk of the save file's size and carry nothing we can use on the
way back in."
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))
                    register-alist
                    (cl-loop for (reg . item) in register-alist
                             if (stringp item)
                             collect (cons reg (substring-no-properties item))
                             else collect (cons reg item)))))
  (add-hook 'savehist-save-hook
            (lambda ()
              "Drop registers holding unwritable values (window configurations).
savehist discards `register-alist' wholesale otherwise. Set buffer-locally: this
hook runs in savehist's temp buffer, so the live session keeps its registers."
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist))))
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
