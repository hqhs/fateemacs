;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-term.el --- ERT tests for the ghostel terminal wiring

;; The point of these is the "optional" contract: lisp/term.el must never pull
;; ghostel in at load time, because ghostel.el runs `ghostel--load-module' as a
;; top-level form. If that contract breaks, a clone without a compiled module
;; fails at startup instead of when the terminal is first asked for.

(require 'ert)

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

;;; test-term.el ends here
