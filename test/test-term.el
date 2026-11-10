;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-term.el --- ERT tests for the ghostel terminal wiring

;; The point of these is the "optional" contract: lisp/term.el must never pull
;; ghostel in at load time, because ghostel.el runs `ghostel--load-module' as a
;; top-level form. If that contract breaks, a clone without a compiled module
;; fails at startup instead of when the terminal is first asked for.

(require 'ert)
(require 'cl-lib)

(defvar test-fate--repo-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;;; Load the code under test. `fate-vendor-dir' normally comes from
;;; lisp/straight.el, which we do not want here -- it would put every vendored
;;; package on the load-path and defeat the point of `emacs -Q'.
(defvar fate-vendor-dir (expand-file-name "vendor/" test-fate--repo-dir))
(require 'use-package)
(load (expand-file-name "lisp/term.el" test-fate--repo-dir) nil t)

(ert-deftest test-fate/term-does-not-load-ghostel-eagerly ()
  "Loading lisp/term.el must not pull in ghostel.
ghostel.el calls `ghostel--load-module' at load time, so requiring it
eagerly would make a missing module a startup failure."
  (should-not (featurep 'ghostel))
  (should-not (featurep 'evil-ghostel)))

(ert-deftest test-fate/term-never-auto-installs ()
  "Auto-install must stay off so ghostel never downloads a binary."
  (should (eq ghostel-module-auto-install nil))
  (should (equal ghostel-module-directory
                 (expand-file-name "ghostel/" fate-vendor-dir))))

(ert-deftest test-fate/term-available-p-tracks-the-module-file ()
  "Availability is exactly \"the built module is on disk\"."
  (let ((+fate-ghostel-module (expand-file-name "definitely-absent.so"
                                                temporary-file-directory)))
    (should-not (+fate/term-available-p)))
  (let ((tmp (make-temp-file "fate-term-module")))
    (unwind-protect
        (let ((+fate-ghostel-module tmp))
          ;; Only meaningful on an Emacs built --with-modules; without it the
          ;; terminal cannot work regardless of what is on disk.
          (should (eq (and (+fate/term-available-p) t)
                      (and module-file-suffix t))))
      (delete-file tmp))))

(ert-deftest test-fate/term-signals-user-error-without-module ()
  "With no module, `+fate/term' explains itself instead of erroring deep
inside `module-load'."
  (let ((+fate-ghostel-module (expand-file-name "definitely-absent.so"
                                                temporary-file-directory)))
    (let ((err (should-error (+fate/term) :type 'user-error)))
      (should (string-match-p "build-term\\.sh" (error-message-string err))))))

(ert-deftest test-fate/project-term-signals-user-error-without-module ()
  "`+fate/project-term' carries the same guard as `+fate/term'.
Without it, `SPC p s' would fail inside `module-load' on a clone that has
never run build-term.sh."
  (let ((+fate-ghostel-module (expand-file-name "definitely-absent.so"
                                                temporary-file-directory)))
    (let ((err (should-error (+fate/project-term) :type 'user-error)))
      (should (string-match-p "build-term\\.sh" (error-message-string err))))
    ;; ...and it must bail out *before* reaching for ghostel.
    (should-not (featurep 'ghostel-project))))

;;; Evil wiring. Both of these regressed once: requiring evil-ghostel does not
;;; enable it, and the global `jk' escape shadows j/k in every insert state --
;;; including a terminal buffer, where the handler's `insert' writes into
;;; renderer-owned text and the keystroke never reaches the shell.
;;;
;;; `+fate--term-setup-evil' exists so this is checkable under `emacs -Q':
;;; stub the two symbols it touches and assert what it did. They need real
;;; `defvar's -- `let' over an undefined symbol binds lexically, which the
;;; function under test would not see.

(defvar ghostel-mode-hook nil)
(defvar evil-ghostel-mode-map (make-sparse-keymap))

(defun test-fate--plist-get-equal (plist key)
  "Like `plist-get', but compare keys with `equal'; here they are key vectors."
  (let (found)
    (while (and plist (not found))
      (if (equal (car plist) key)
          (setq found (cadr plist))
        (setq plist (cddr plist))))
    found))

(defun test-fate--capture-setup-evil ()
  "Run the evil wiring with `evil-define-key*' stubbed; return its args."
  (let ((calls nil))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (&rest args) (push args calls))))
      (+fate--term-setup-evil))
    (car calls)))

(ert-deftest test-fate/term-setup-evil-enables-mode-per-buffer ()
  "The wiring must hook `evil-ghostel-mode' onto `ghostel-mode'.
It is a plain buffer-local minor mode with no self-enabling hook, so
`require' alone leaves the entire evil integration inert."
  (let ((ghostel-mode-hook nil))
    (test-fate--capture-setup-evil)
    (should (memq 'evil-ghostel-mode ghostel-mode-hook))))

(ert-deftest test-fate/term-setup-evil-unshadows-j-and-k ()
  "j and k must be rebound for insert state so they reach the terminal.
They have to land on `self-insert-command', not on ghostel's private
sender, so they flow through ghostel's own `[remap self-insert-command]'
like every other printable key."
  (let* ((ghostel-mode-hook nil)
         (call (test-fate--capture-setup-evil)))
    (should call)
    ;; (STATE MAP KEY DEF KEY DEF ...)
    (should (eq (nth 0 call) 'insert))
    (should (eq (nth 1 call) evil-ghostel-mode-map))
    (let ((bindings (nthcdr 2 call)))
      (should (eq (test-fate--plist-get-equal bindings (kbd "j"))
                  'self-insert-command))
      (should (eq (test-fate--plist-get-equal bindings (kbd "k"))
                  'self-insert-command)))))

;;; The terminal tab. `+fate/term' is a toggle, not an opener: the terminal
;;; owns one tab-bar tab and the key flips in and out of it. Both halves are
;;; checked with `ghostel' and the tab-bar commands stubbed, so none of this
;;; needs a compiled module or a real frame.

(defun test-fate--tabs (&rest names)
  "Return a `tab-bar-tabs-function' value naming NAMES, first one selected."
  (let ((i 0))
    (mapcar (lambda (name)
              (setq i (1+ i))
              `(,(if (= i 1) 'current-tab 'tab) (name . ,name)))
            names)))

(defmacro test-fate--with-term-module (&rest body)
  "Run BODY with the module reported present and `ghostel' stubbed.
Collects every stubbed call into `calls', newest last."
  `(let ((calls nil))
     (cl-letf (((symbol-function '+fate/term-available-p) (lambda () t))
               ;; Nothing is on the load-path under `emacs -Q', so the real
               ;; `require' would fail. A let-bound `features' does not fool
               ;; it, hence the stub; recording the call keeps "loads ghostel
               ;; before calling into it" an assertion rather than a hope.
               ((symbol-function 'require)
                (lambda (feature &rest _)
                  (setq calls (append calls `((require ,feature))))))
               ((symbol-function 'ghostel)
                (lambda (&rest _) (setq calls (append calls '((ghostel))))))
               ((symbol-function 'tab-bar-switch-to-tab)
                (lambda (name) (setq calls (append calls `((switch ,name))))))
               ((symbol-function 'tab-bar-switch-to-recent-tab)
                (lambda (&rest _) (setq calls (append calls '((recent)))))))
       ,@body
       calls)))

(ert-deftest test-fate/term-tab-p-reads-the-selected-tab ()
  "The terminal tab is the selected one iff its name matches."
  (let ((tab-bar-tabs-function
         (lambda () (test-fate--tabs +fate-term-tab-name "editor"))))
    (should (+fate--term-tab-p)))
  (let ((tab-bar-tabs-function
         (lambda () (test-fate--tabs "editor" +fate-term-tab-name))))
    (should-not (+fate--term-tab-p))))

(ert-deftest test-fate/term-switches-to-the-term-tab ()
  "From anywhere else, go to the terminal tab and (re)attach the buffer.
`tab-bar-switch-to-tab' creates the tab when missing, so one call covers
both first use and coming back; `ghostel' then reuses or respawns the
buffer, which is what repairs a tab whose terminal was killed."
  (let ((tab-bar-tabs-function (lambda () (test-fate--tabs "editor"))))
    (should (equal (test-fate--with-term-module (+fate/term))
                   `((require ghostel)
                     (switch ,+fate-term-tab-name)
                     (ghostel))))))

(ert-deftest test-fate/term-toggles-back-out-of-the-term-tab ()
  "From inside the terminal tab, the same key returns to the previous tab.
It must not start another terminal on the way out."
  (let ((tab-bar-tabs-function
         (lambda () (test-fate--tabs +fate-term-tab-name "editor"))))
    (should (equal (test-fate--with-term-module (+fate/term))
                   '((recent))))))

;;; test-term.el ends here
