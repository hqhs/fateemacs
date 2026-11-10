;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-whitespace.el --- ERT tests: edited-lines-only whitespace cleanup

;; The cleanup must behave like ws-butler: trailing whitespace on lines we
;; touched gets deleted, trailing whitespace on lines we never looked at is
;; left alone (otherwise one save in somebody else's repo produces a diff full
;; of whitespace hunks).

(require 'ert)

(defvar +test-emacs-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; `prog-conf.el' as a whole pulls in treesit config and vendored packages, so
;; lift out just the whitespace machinery.
(let ((file (expand-file-name "lisp/prog-conf.el" +test-emacs-dir)))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defvar-local defun))
                         (string-prefix-p "+fate--ws-" (symbol-name (cadr form))))
                (push form forms))))
        (end-of-file nil))
      (unless forms
        (error "No +fate--ws-* definitions found in %s" file))
      (mapc #'eval (nreverse forms)))))

(defmacro +test-with-ws-buffer (contents &rest body)
  "Run BODY in a temp buffer holding CONTENTS with the ws hooks wired up."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (+fate--ws-setup)
     ;; `+fate--ws-setup' puts `+fate--ws-cleanup' on a buffer-local
     ;; `before-save-hook'; there is no file here, so call it directly.
     (setq +fate--ws-regions nil)
     ,@body))

(ert-deftest test-ws-cleanup-leaves-untouched-lines ()
  "Trailing whitespace on a line we never edited survives a save."
  (+test-with-ws-buffer "one   \ntwo\nthree\nfour\nfive\n"
    (goto-char (point-min))
    (forward-line 4)                    ; line 5
    (end-of-line)
    (insert "   ")                      ; dirty line 5 only
    (+fate--ws-cleanup)
    (should (equal (buffer-string) "one   \ntwo\nthree\nfour\nfive\n"))))

(ert-deftest test-ws-cleanup-cleans-edited-line ()
  "Trailing whitespace on an edited line is deleted."
  (+test-with-ws-buffer "alpha\nbeta\n"
    (goto-char (point-min))
    (end-of-line)
    (insert "  \t ")
    (should (equal (buffer-string) "alpha  \t \nbeta\n"))
    (+fate--ws-cleanup)
    (should (equal (buffer-string) "alpha\nbeta\n"))))

(ert-deftest test-ws-cleanup-covers-whole-edited-line ()
  "An edit mid-line still owns the tail of its line."
  (+test-with-ws-buffer "alpha   \nbeta   \n"
    (goto-char (point-min))
    (forward-char 2)
    (insert "X")                        ; mid-line edit on line 1
    (+fate--ws-cleanup)
    (should (equal (buffer-string) "alXpha\nbeta   \n"))))

(ert-deftest test-ws-cleanup-resets-regions ()
  "The region list is emptied (and its markers released) after a cleanup."
  (+test-with-ws-buffer "x\n"
    (goto-char (point-max))
    (insert "y  \n")
    (should +fate--ws-regions)
    (+fate--ws-cleanup)
    (should-not +fate--ws-regions)))

(provide 'test-whitespace)
