;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-so-long.el --- ERT tests: the so-long long-lines/long-file split

(require 'ert)
(require 'so-long)

(defvar +test-emacs-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; `defaults.el' as a whole needs the full init environment, so lift out just
;; the so-long predicate and its two companions.
(let ((file (expand-file-name "lisp/defaults.el" +test-emacs-dir)))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defvar defvar-local defun put))
                         (string-match-p "so-long\\|file-lines-threshold"
                                         (format "%S" (cadr form))))
                (push form forms))))
        (end-of-file nil))
      (unless forms
        (error "No so-long definitions found in %s" file))
      (mapc #'eval (nreverse forms)))))

(defmacro +test-with-file-buffer (name &rest body)
  "Run BODY in a scratch buffer pretending to visit NAME.
Deliberately not `with-temp-buffer': that names its buffer \" *temp*\", and a
leading space is exactly what the predicate treats as behind-the-scenes."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "fate-so-long-test")))
     (unwind-protect
         (with-current-buffer buf
           (setq buffer-file-name ,name)
           (unwind-protect (progn ,@body)
             ;; Don't let `kill-buffer' try to deal with a phantom file.
             (setq buffer-file-name nil)))
       (kill-buffer buf))))

(ert-deftest test-so-long-p-detects-long-lines ()
  "A single excessively long line dispatches to the full `so-long-mode'."
  (+test-with-file-buffer "/tmp/long.txt"
    (insert (make-string (* 2 so-long-threshold) ?x) "\n")
    (should (eq (+fate--so-long-p) 'so-long-mode))))

(ert-deftest test-so-long-p-detects-many-lines ()
  "A long file of short lines dispatches to `so-long-minor-mode'."
  (+test-with-file-buffer "/tmp/big.c"
    (insert (mapconcat #'identity
                       (make-list (+ +fate-file-lines-threshold 10) "int x;")
                       "\n"))
    (should (eq (+fate--so-long-p) 'so-long-minor-mode))))

(ert-deftest test-so-long-p-ignores-ordinary-buffers ()
  "A normal file needs no action."
  (+test-with-file-buffer "/tmp/small.c"
    (insert "int main(void) { return 0; }\n")
    (should-not (+fate--so-long-p))))

(ert-deftest test-so-long-p-ignores-non-file-buffers ()
  "Stock so-long fires in magit status and friends; we don't."
  (with-temp-buffer
    (insert (make-string (* 2 so-long-threshold) ?x))
    (should-not (+fate--so-long-p))))

(ert-deftest test-so-long-p-ignores-temp-buffers ()
  "A leading space in the buffer name marks a behind-the-scenes buffer."
  (let ((buf (generate-new-buffer " *fate-so-long-test*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-file-name "/tmp/hidden.txt")
          (insert (make-string (* 2 so-long-threshold) ?x))
          (should-not (+fate--so-long-p))
          (setq buffer-file-name nil))
      (kill-buffer buf))))

(ert-deftest test-so-long-p-ignores-special-modes ()
  "`special-mode' derivatives are not ours to rewrite."
  (+test-with-file-buffer "/tmp/special.txt"
    (insert (make-string (* 2 so-long-threshold) ?x))
    (let ((inhibit-read-only t))
      (special-mode))
    (setq buffer-file-name "/tmp/special.txt")
    (should-not (+fate--so-long-p))))

(ert-deftest test-so-long-kind-is-cached ()
  "`+fate--so-long-p' records its decision for the activate function."
  (+test-with-file-buffer "/tmp/big.c"
    (insert (mapconcat #'identity
                       (make-list (+ +fate-file-lines-threshold 10) "int x;")
                       "\n"))
    (+fate--so-long-p)
    (should (eq +fate--so-long-kind 'so-long-minor-mode))))

(provide 'test-so-long)
