;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-escape.el --- ERT tests: the do-what-I-mean escape handler

(require 'ert)
(require 'cl-lib)

(defvar +test-emacs-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; `evil.el' needs evil itself, which `emacs -Q --batch' does not have on its
;; load-path. Lift out just the hook variable and the command.
(let ((file (expand-file-name "lisp/evil.el" +test-emacs-dir)))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defvar defun))
                         (memq (cadr form) '(+fate-escape-hook +fate/escape)))
                (push form forms))))
        (end-of-file nil))
      (should (= (length forms) 2))
      (mapc #'eval (nreverse forms)))))

(ert-deftest test-escape-runs-hook-in-order-and-stops ()
  "The first non-nil hook function wins; later ones don't run."
  (let ((log nil)
        (+fate-escape-hook nil))
    (add-hook '+fate-escape-hook (lambda () (push 'third log) t))
    (add-hook '+fate-escape-hook (lambda () (push 'second log) t))
    (add-hook '+fate-escape-hook (lambda () (push 'first log) nil))
    (+fate/escape)
    (should (equal (nreverse log) '(first second)))))

(ert-deftest test-escape-falls-through-to-keyboard-quit ()
  "With nothing to quit, the handler reaches `keyboard-quit'."
  (let ((+fate-escape-hook nil)
        (quit-called nil))
    (cl-letf (((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t))))
      (+fate/escape))
    (should quit-called)))

(ert-deftest test-escape-does-not-abort-macro-recording ()
  "C-g while recording a macro must not reach `keyboard-quit'."
  (let ((+fate-escape-hook nil)
        (quit-called nil)
        (defining-kbd-macro t))
    (cl-letf (((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t))))
      (+fate/escape))
    (should-not quit-called))
  (let ((+fate-escape-hook nil)
        (quit-called nil)
        (executing-kbd-macro "x"))
    (cl-letf (((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t))))
      (+fate/escape))
    (should-not quit-called)))

(ert-deftest test-escape-hook-still-runs-during-macro ()
  "The macro guard sits *after* the hook: quitting state is still allowed."
  (let ((ran nil)
        (+fate-escape-hook nil)
        (defining-kbd-macro t))
    (add-hook '+fate-escape-hook (lambda () (setq ran t)))
    (+fate/escape)
    (should ran)))

(ert-deftest test-escape-aborts-minibuffer ()
  "An active minibuffer is aborted before the hook is consulted."
  (let ((ran nil)
        (+fate-escape-hook nil)
        (aborted nil))
    (add-hook '+fate-escape-hook (lambda () (setq ran t) t))
    (cl-letf (((symbol-function 'minibuffer-window-active-p) (lambda (_) t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (setq aborted t))))
      (+fate/escape))
    (should aborted)
    (should-not ran)))

(provide 'test-escape)
