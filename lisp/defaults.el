;; -*- lexical-binding: t -*-
;; better values for emacs out-of-the-box configuration options

;; `save-buffers-kill-emacs' offers to save modified buffers before consulting
;; this, so anything still modified by the time we are asked is work the user
;; declined to save. A session with nothing outstanding has nothing to confirm.
(defun +fate-quit-p (&optional prompt)
  "Confirm quitting Emacs, but only when there is unsaved work."
  (if (cl-find-if (lambda (buf)
                    (and (buffer-file-name buf) (buffer-modified-p buf)))
                  (buffer-list))
      (y-or-n-p (or prompt "Quit Emacs without saving? "))
    t))
(setq confirm-kill-emacs #'+fate-quit-p)

;;; PATH
;;
;; This used to be `(split-string (getenv "PATH") ...)', which despite the "set
;; PATH from shell" comment only copied whatever PATH Emacs happened to
;; inherit. Started from a terminal that is the shell's PATH and everything
;; works. Started from the Dock or via `open -a', launchd hands over a bare
;; /usr/bin:/bin:/usr/sbin:/sbin -- so no rg, no clangd, no cargo, and
;; `executable-find' says they aren't installed.
;;
;; So ask the login shell instead. `-i' as well as `-l', because PATH
;; additions conventionally live in .zshrc rather than .zprofile (ours do:
;; ~/.local/bin is only on the interactive PATH). Measured at ~40ms, and only
;; paid when there is no shell environment to inherit in the first place.

(defun +fate--login-shell-path ()
  "Return PATH as the user's login shell sees it, or nil if it can't be read."
  (let* ((shell (or (getenv "SHELL") shell-file-name))
         (marker "__fate_path__=")
         ;; DESTINATION discards stderr: a chatty rc file is not our problem.
         (output (with-output-to-string
                   (call-process shell nil (list standard-output nil) nil
                                 "-lic"
                                 (concat "printf '" marker "%s' \"$PATH\"")))))
    ;; An interactive shell may print a banner first; the marker is how we find
    ;; our own output again.
    (when (string-match (concat (regexp-quote marker) "\\(.+\\)") output)
      (match-string 1 output))))

(defun +fate--set-exec-path ()
  "Set `exec-path' and $PATH, consulting the login shell when there is no
shell environment to inherit (a GUI app launch, or a daemon)."
  (let ((path (or (and (eq system-type 'darwin)
                       (or (daemonp) (display-graphic-p))
                       (+fate--login-shell-path))
                  (getenv "PATH"))))
    ;; Both: `exec-path' is what `executable-find' and `call-process' use,
    ;; $PATH is what anything spawned through a shell uses (compile, eglot).
    (setenv "PATH" path)
    (setq-default exec-path (append (split-string path path-separator t)
                                    (list exec-directory)))))

(+fate--set-exec-path)

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

;; Enriched text is a historical code-execution vector via .rtf/.enriched. The
;; two forms below are the whole mitigation: neutering the display-property
;; decoder, and taking .rtf out of `auto-mode-alist'. Flag variables like
;; `enriched-mode' or `auto-image-file-mode' do nothing here -- they are
;; buffer-local minor-mode flags that are already nil, and setting them does
;; not call the mode function. (`inhibit-images' does not exist in any Emacs.)
;; Don't re-add them; they read as protection and provide none.
(with-eval-after-load 'enriched
  (defun enriched-decode-display-prop (_start _end &optional _param) nil))
(setq auto-mode-alist (assoc-delete-all "\\.rtf\\'" auto-mode-alist #'string=))

;; Emacs 30+. `trusted-content' keeps our own elisp out of the untrusted bucket
;; (which otherwise disables things like flymake's byte-compile backend).
;; `safe-local-variable-directories' is the targeted alternative to relaxing
;; `enable-local-variables' globally: listed roots are trusted outright, so
;; per-project `eglot-server-programs' work without `:all' everywhere.
;; `enable-local-eval' nil still blocks `eval:' forms in those directories --
;; that combination is the intent.
(let ((dir (abbreviate-file-name (file-truename fate-emacs-dir))))
  (when (boundp 'trusted-content)
    (add-to-list 'trusted-content dir))
  (when (boundp 'safe-local-variable-directories)
    (add-to-list 'safe-local-variable-directories dir)))

;; These are the entries that Customize accumulated in init.el before
;; `custom-file' was set (see below). Hand-written here because cache/ is
;; gitignored and a fresh clone must still carry them.
(setq safe-local-variable-values
      '((eglot-connect-timeout . 120)
        (eglot-ignored-server-capabilities . t)
        (apheleia-inhibit . t)
        (projectile-project-compilation-cmd
         . "scons --gpu=wgpu-native --editor pizza && ./build/pizza")
        (projectile-project-compilation-cmd
         . "scons --gpu=wgpu-native --editor thegame && ./build/thegame")
        (eglot-server-programs
         (c-mode "clangd" "--background-index" "--clang-tidy"
                 "--header-insertion=never" "--completion-style=detailed"
                 "--function-arg-placeholders" "--fallback-style=llvm")
         (c++-mode "clangd" "--background-index" "--clang-tidy"
                   "--header-insertion=never" "--completion-style=detailed"
                   "--function-arg-placeholders" "--fallback-style=llvm"))))

;; Anything calling `customize-save-variable' writes to `user-init-file' while
;; this is nil -- that is how a `custom-set-variables' block grew in init.el.
;; Give it a gitignored scratch file instead; init.el loads it last.
(setq custom-file (expand-file-name "custom.el" fate-cache-dir))

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

;;; State files
;; Everything below defaults to somewhere in `fate-emacs-dir', i.e. into the
;; working tree. `tramp' was actually tracked in git, so every TRAMP connection
;; dirtied the checkout. Both packages are lazy, so setting these here is early
;; enough.
(setq tramp-persistency-file-name (expand-file-name "tramp" fate-cache-dir)
      bookmark-default-file (expand-file-name "bookmarks" fate-cache-dir))

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

;; Menu/tool/scroll bars are already configured in early-init.el via
;; default-frame-alist. Set mode vars to match so toggling works correctly.
;; The menu bar is deliberately left on for graphical frames on macOS -- see
;; the HACK comment in early-init.el.
(setq menu-bar-mode (and (eq system-type 'darwin) (display-graphic-p))
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
;;
;; NOTE(hqhs): `:init (setq ...)' rather than `:custom' in every deferred block
;; below. `:custom' expands to `custom-theme-set-variables', which calls
;; `custom-load-symbol' for any variable whose `defcustom' has not run yet --
;; i.e. it loads the package to set the option, defeating the deferral.
(use-package autorevert
  :ensure nil ;; built-in
  :defer t
  :init
  (setq auto-revert-verbose t             ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        revert-without-query (list "."))  ; only prompt when the buffer is unsaved

  (defun +fate--auto-revert-wire-h ()
    "Wire up lazy autorevert. Runs once, on the first file visited."
    (require 'autorevert)
    ;; Buffer shown in a window / window selected / anything saved / frame
    ;; refocused after using another app.
    (add-hook 'window-buffer-change-functions #'+fate--auto-revert-visible-buffers-h)
    (add-hook 'window-selection-change-functions #'+fate--auto-revert-visible-buffers-h)
    (add-hook 'after-save-hook #'+fate--auto-revert-visible-buffers-h)
    (add-function :after after-focus-change-function
                  #'+fate--auto-revert-visible-buffers-h))
  (add-hook '+fate-first-file-hook #'+fate--auto-revert-wire-h)
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
        (+fate--auto-revert-buffer-h)))))

(use-package saveplace
  :ensure nil ;; built-in
  :defer t
  :init
  (setq save-place-file (expand-file-name "saveplace" fate-cache-dir))
  (add-hook '+fate-first-input-hook #'save-place-mode)
  :config
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
  :defer t
  :init
  (add-hook '+fate-first-input-hook #'which-key-mode)
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.1
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil ; jarring to separate keys by case
        which-key-add-column-padding 1     ; less packed UI
        which-key-min-display-lines 7      ; prevent a short+wide which-key pane
        which-key-side-window-slot -10     ; don't replace popups
        which-key-compute-remaps t         ; show remapped commands
        which-key-ellipsis "…"))

(use-package uniquify
  :ensure nil ;; built-in
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :ensure nil ;; built-in
  :defer t
  :init
  (add-hook '+fate-first-file-hook #'recentf-mode)
  (setq recentf-max-saved-items 200
        recentf-save-file (expand-file-name "recentf" fate-cache-dir)
        ;; Cleaning up on a timer stats every remembered file while you are
        ;; working. Quitting is the one moment when that latency costs nothing.
        recentf-auto-cleanup 'never)
  :config
  ;; Text properties inflate the save file for no benefit. Must come first in
  ;; `recentf-filename-handlers' -- `add-to-list' prepends.
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  ;; Negative depth so the cleanup runs before `recentf-save-list', which
  ;; `recentf-mode' puts on `kill-emacs-hook' at the default depth.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -50))

(use-package savehist
  :ensure nil ;; built-in
  :defer t
  :init
  (add-hook '+fate-first-input-hook #'savehist-mode)
  (setq savehist-file (expand-file-name "savehist" fate-cache-dir)
        history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables
        '(kill-ring                     ; persist clipboard
          register-alist                ; persist macros
          mark-ring global-mark-ring    ; persist marks
          search-ring regexp-search-ring)) ; persist searches
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
                          (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package repeat
  :ensure nil ;; built-in
  :custom
  (repeat-mode 1)
  (repeat-exit-key (kbd "RET"))
  (repeat-exit-timeout 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; so-long: two different problems, two different answers
;;
;; `global-so-long-mode' is enabled in init.el (eagerly, see the comment
;; there). Stock behaviour makes a triggering buffer read-only with font-lock
;; and line numbers off, which is right for a minified blob and a wall for a
;; 30k-line generated source file. Split the two cases.

(defvar +fate-file-lines-threshold 20000
  "Line count above which a buffer gets `so-long-minor-mode'.
Flat value: this is a native-comp build, so Doom's scaling branches don't
apply.")

(defvar-local +fate--so-long-kind nil
  "What `+fate--so-long-p' last decided for this buffer.")
(put '+fate--so-long-kind 'permanent-local t)

(defun +fate--so-long-p ()
  "Decide whether and how the current buffer needs so-long handling.
Returns `so-long-mode' for excessively long lines, `so-long-minor-mode' for a
buffer that is merely very long, and nil otherwise. Stock `so-long' fires
inside magit status and other special or temporary buffers; those are excluded
here."
  (setq +fate--so-long-kind
        (and buffer-file-name
             (not (string-prefix-p " " (buffer-name)))
             (not (derived-mode-p 'special-mode))
             (let ((stats (buffer-line-statistics)))
               ;; (LINES LONGEST-LINE-BYTES MEAN)
               (cond ((> (nth 1 stats) so-long-threshold) 'so-long-mode)
                     ((> (nth 0 stats) +fate-file-lines-threshold)
                      'so-long-minor-mode))))))

(defun +fate--so-long-activate ()
  "Dispatch on `+fate--so-long-p': major mode for long lines, minor for long files."
  (if (eq (or +fate--so-long-kind (+fate--so-long-p)) 'so-long-mode)
      ;; Minified. Nothing about this file is meant to be read, so the stock
      ;; action -- read-only, no font-lock, no line numbers -- is correct.
      (so-long-mode)
    ;; A big but otherwise normal source file. Keep highlighting, line numbers
    ;; and editing; only drop the per-line machinery that does not scale.
    ;;
    ;; `so-long-override-variables' reads the *remembered* lists rather than
    ;; the live ones, and `so-long-remember-all' already ran before this
    ;; function was called -- hence re-remembering after the buffer-local
    ;; tweaks below.
    (setq-local so-long-minor-modes
                (seq-difference so-long-minor-modes
                                '(font-lock-mode display-line-numbers-mode))
                so-long-variable-overrides
                (append '(;; Cheaper highlighting instead of none.
                          (font-lock-maximum-decoration . 1)
                          ;; Stop saveplace thrashing on a file this size.
                          (save-place-alist . nil))
                        (assq-delete-all
                         'buffer-read-only
                         (copy-alist so-long-variable-overrides))))
    (so-long-remember-all :reset)
    (turn-on-so-long-minor-mode)))

(defun +fate--so-long-revert ()
  "Undo whichever action `+fate--so-long-activate' took."
  (if (eq +fate--so-long-kind 'so-long-mode)
      (so-long-mode-revert)
    (turn-off-so-long-minor-mode)))

(use-package so-long
  :ensure nil ;; built-in
  :defer t
  :config
  (add-to-list 'so-long-action-alist
               '(+fate-so-long
                 "Fate: minor mode for long files, major mode for long lines"
                 +fate--so-long-activate
                 +fate--so-long-revert))
  (setq so-long-action '+fate-so-long
        so-long-predicate #'+fate--so-long-p)
  ;; Config files and prose get just as slow as code does.
  (dolist (mode '(conf-mode text-mode))
    (add-to-list 'so-long-target-modes mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer

;; Without this you cannot invoke a command from inside a prompt.
(setq enable-recursive-minibuffers t
      ;; Hide commands irrelevant to the current mode from `M-x'.
      read-extended-command-predicate #'command-completion-default-include-p
      ;; vim's `showcmd'. With `SPC' as the leader and which-key on a 1.0s
      ;; delay, a partial `SPC g ...' otherwise gives no feedback at all for a
      ;; full second.
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      ;; Keep point out of the read-only prompt -- relevant given the C-a/C-e
      ;; bindings in insert state.
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS

(when (eq system-type 'darwin)
  (setq ns-command-modifier 'super
        ns-option-modifier 'meta
        ;; Free the right option key for character composition.
        ns-right-option-modifier 'none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; `kill-ring' is persisted through savehist, so duplicates bloat the save file.
(setq kill-do-not-save-duplicates t
      find-file-suppress-same-file-warnings t
      confirm-nonexistent-file-or-buffer nil
      mouse-yank-at-point t
      ;; Keep `untabify'/`tabify' out of the middle of a line.
      tabify-regexp "^\t* [ \t]+"
      ;; `truncate-lines' is already t (prog-conf.el), so honouring this is
      ;; work Emacs does for nothing.
      truncate-partial-width-windows nil
      ;; Absolute numbers inside a narrowed region, so it is visible that the
      ;; buffer *is* narrowed.
      display-line-numbers-widen t
      ;; The companion to our 1MB `read-process-output-max', and the knob that
      ;; actually reduces eglot latency: without it Emacs throttles reads from
      ;; a chatty process regardless of how big the buffer is.
      process-adaptive-read-buffering nil)

;; Offer to create the parent directory rather than erroring out.
(defun +fate--create-missing-directories-h ()
  "Create a new file's parent directory, with confirmation."
  (unless (file-remote-p buffer-file-name)
    (let ((parent (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent))
           (y-or-n-p (format "Directory `%s' does not exist. Create it? " parent))
           (progn (make-directory parent 'parents) t)))))
(add-hook 'find-file-not-found-functions #'+fate--create-missing-directories-h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE-local-vars-hook
;;
;; Mode hooks run *before* directory-local variables are applied, so anything
;; wired to a plain mode hook that reads one sees the global value. Directly
;; relevant here: `safe-local-variable-values' above carries per-project
;; `eglot-server-programs' and `eglot-connect-timeout', which an `eglot-ensure'
;; on `c-ts-mode-hook' would never see. These synthesise a
;; `MAJOR-MODE-local-vars-hook' that runs after `hack-local-variables'.

(defvar-local +fate--local-var-hooks-ran nil)

(defun +fate-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook for the current major mode."
  (unless (or +fate--local-var-hooks-ran delay-mode-hooks)
    (setq-local +fate--local-var-hooks-ran t)
    (when-let* ((hook (intern-soft (format "%s-local-vars-hook" major-mode))))
      (run-hooks hook))))

(defun +fate-run-local-var-hooks-maybe-h ()
  "Trigger `+fate-run-local-var-hooks-h' when dir-locals are disabled.
`hack-local-variables-hook' never fires in that case."
  (unless enable-local-variables
    (+fate-run-local-var-hooks-h)))

(add-hook 'hack-local-variables-hook #'+fate-run-local-var-hooks-h)
(add-hook 'after-change-major-mode-hook #'+fate-run-local-var-hooks-maybe-h 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint (project-shell, <leader>ps)

;; Past output is still editable; this just stops the prompt itself from being
;; mangled. `comint-buffer-maximum-size' is doubled from the default and is
;; also what bounds compilation buffers (see lisp/prog-conf.el).
(setq comint-prompt-read-only t
      comint-buffer-maximum-size 2048)

(defun +fate--comint-move-to-prompt-h ()
  "Move point to the prompt before a command that would insert text.
Typing with point in past output otherwise just raises a read-only error."
  (when (and (memq this-command '(self-insert-command
                                  evil-paste-after evil-paste-before
                                  yank yank-pop))
             (< (point) (comint-line-beginning-position)))
    (goto-char (point-max))))

(add-hook 'comint-mode-hook
          (lambda ()
            (add-hook 'pre-command-hook #'+fate--comint-move-to-prompt-h nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window layout undo

(use-package winner
  :ensure nil ;; built-in
  :defer t
  ;; winner.el only autoloads `winner-mode'; the two commands we bind in
  ;; lisp/keybindings.el need their own.
  :commands (winner-undo winner-redo)
  :init
  ;; We bind our own (lisp/keybindings.el); winner's C-c <left>/<right> would
  ;; shadow nothing useful but are not where we look for them.
  (setq winner-dont-bind-my-keys t
        winner-boring-buffers
        '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Apropos*"
          "*Help*" "*Buffer List*" "*Ibuffer*" "*esh command on file*"
          "*cvs*" "*Warnings*"))
  (add-hook '+fate-first-file-hook #'winner-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server, for emacsclient / $EDITOR / git commit

(use-package server
  :ensure nil ;; built-in
  :defer t
  :init
  ;; `server-auth-dir' reads `user-emacs-directory', i.e. this repo.
  (setq server-auth-dir (expand-file-name "server/" fate-cache-dir))
  (defun +fate--start-server-h ()
    "Start the edit server, but only for a real GUI session."
    (when (display-graphic-p)
      (require 'server)
      (when-let* ((name (getenv "EMACS_SERVER_NAME")))
        (setq server-name name))
      (unless (server-running-p)
        (server-start nil t))))
  (add-hook 'emacs-startup-hook #'+fate--start-server-h))

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
