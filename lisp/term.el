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
  (+fate--term-setup-tramp-guards)
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

;;; Keeping a local terminal local
;;
;; Ghostel turns every OSC 7 directory report whose hostname it doesn't
;; recognise into a TRAMP `default-directory' -- deliberately without
;; verifying it, so `cd' stays fast. `ghostel--local-host-p' recognises only
;; `localhost' and Emacs' `(system-name)', which is frozen at startup, while
;; the macOS kernel hostname flips between the DHCP-supplied name and
;; `<LocalHostName>.local' as network state changes. Start Emacs on one
;; network, move to another, and a purely local terminal claims to live on
;; `/ssh:<this-very-mac>:'.
;;
;; That alone would be cosmetic, except `process-attributes' is a
;; file-name-handler operation dispatched off `default-directory'. So the pid
;; lookups in `ghostel--shell-pgid' and `ghostel--notify-foreground-change'
;; get handed to TRAMP, which opens an ssh connection to run `ps' on "the
;; remote" -- synchronously, on `kill-buffer', via
;; `ghostel--kill-buffer-query' -> `ghostel-command-running-p'. Killing a
;; terminal then hangs for the ssh timeout.
;;
;; Two fixes, because either alone leaves a hole: widen the set of names
;; treated as local, and stop the pid lookups from going through TRAMP at all
;; (they read the *local* pty's process group even when the shell inside it
;; really has ssh'd somewhere -- see `ghostel-foreground-pid').

(defvar +fate--term-local-hostnames nil
  "Cache for `+fate--term-local-hostnames'.")

(defvar +fate--term-local-hostnames-time nil
  "`float-time' when `+fate--term-local-hostnames' was last filled.")

(defconst +fate--term-local-hostnames-ttl 60
  "Seconds a cached hostname set stays fresh.
Long enough that a real ssh session's every `cd' doesn't shell out
twice, short enough that a network change is noticed promptly.")

(defun +fate--term-strip-dot-local (host)
  "Return HOST downcased, with a trailing \".local\" (mDNS) removed."
  (let ((host (downcase host)))
    (if (string-suffix-p ".local" host)
        (substring host 0 (- (length host) (length ".local")))
      host)))

(defun +fate--term-local-command-output (program &rest args)
  "Return the first line PROGRAM prints with ARGS, or nil.
`default-directory' is forced local: the caller runs inside a ghostel
buffer, whose directory is exactly the bogus TRAMP path at issue here."
  (let ((default-directory temporary-file-directory))
    (when (executable-find program)
      (with-temp-buffer
        (when (eq 0 (ignore-errors
                      (apply #'call-process program nil '(t nil) nil args)))
          (goto-char (point-min))
          (let ((line (string-trim (buffer-substring-no-properties
                                    (point) (line-end-position)))))
            (unless (string-empty-p line) line)))))))

(defun +fate--term-local-hostnames ()
  "Return the names this machine answers to right now.
Downcased with \".local\" stripped, to match `ghostel--local-host-p'."
  (let ((now (float-time)))
    (when (or (null +fate--term-local-hostnames-time)
              (> (- now +fate--term-local-hostnames-time)
                 +fate--term-local-hostnames-ttl))
      (setq +fate--term-local-hostnames-time now
            +fate--term-local-hostnames
            (delete-dups
             (mapcar
              #'+fate--term-strip-dot-local
              (delq nil
                    (list (system-name)
                          ;; The live kernel hostname, which `(system-name)'
                          ;; stopped tracking when Emacs started.
                          (+fate--term-local-command-output "hostname")
                          ;; The mDNS name, which is what the kernel hostname
                          ;; falls back to once the DHCP-supplied one is gone.
                          (and (eq system-type 'darwin)
                               (+fate--term-local-command-output
                                "scutil" "--get" "LocalHostName")))))))))
  +fate--term-local-hostnames)

(defun +fate--term-local-host-p-a (host)
  "Return non-nil when HOST is another name for this machine.
`:after-until' advice on `ghostel--local-host-p', so ghostel's own cheap
string comparison runs first and the subprocesses here are only paid for
on the path that would otherwise declare HOST remote."
  (and (stringp host)
       (not (string-empty-p host))
       (member (+fate--term-strip-dot-local host)
               (+fate--term-local-hostnames))
       t))

(defun +fate--term-local-default-directory-a (fn &rest args)
  "Apply FN to ARGS with `default-directory' forced local.
Keeps the `process-attributes' calls inside out of TRAMP's hands."
  (let ((default-directory temporary-file-directory))
    (apply fn args)))

(defun +fate--term-setup-tramp-guards ()
  "Stop ghostel from mistaking this machine for a remote one.
Split out of the `ghostel' :config block so it can be tested without
loading the terminal stack."
  (advice-add 'ghostel--local-host-p :after-until
              #'+fate--term-local-host-p-a)
  (dolist (fn '(ghostel--shell-pgid ghostel--notify-foreground-change))
    (advice-add fn :around #'+fate--term-local-default-directory-a)))

(defvar +fate-term-tab-name "term"
  "Name of the tab-bar tab that holds the terminal.")

(defun +fate--term-tab-p ()
  "Return non-nil when the selected tab is the terminal tab.
The selected tab is the one `tab-bar-tabs-function' returns with a
`current-tab' car; its `name' is what `tab-bar-switch-to-tab' matches on."
  (let ((current (seq-find (lambda (tab) (eq (car-safe tab) 'current-tab))
                           (funcall tab-bar-tabs-function))))
    (equal (alist-get 'name current) +fate-term-tab-name)))

(defun +fate/term ()
  "Flip between the terminal tab and the tab you came from.
The terminal gets a tab of its own, `+fate-term-tab-name', created on
first use; from inside it this goes back to the most recent other tab,
so one key is the whole editor <-> terminal round trip. `gt'/`gT' still
walk every tab, terminal included.

Reports what to build when the native module is missing, instead of
failing inside `module-load'."
  (interactive)
  (unless (+fate/term-available-p)
    (user-error
     "No terminal module: run ./scripts/build-term.sh (needs zig)"))
  (if (+fate--term-tab-p)
      ;; Messages "No more recent tabs" when the terminal is the only tab,
      ;; which is the right answer: there is no editor to go back to.
      (tab-bar-switch-to-recent-tab)
    (require 'ghostel)
    ;; Creates the tab and names it when it is not there yet. A new tab starts
    ;; with one window (`tab-bar-new-tab-to' deletes the others), so the
    ;; terminal has it to itself.
    (tab-bar-switch-to-tab +fate-term-tab-name)
    ;; Idempotent -- `ghostel' switches to `ghostel-buffer-name' when it is
    ;; live and starts one otherwise, so this also repairs a terminal tab
    ;; whose buffer was killed out from under it.
    (ghostel)))
