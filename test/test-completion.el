;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-completion.el --- ERT tests for fate completion functions

(require 'ert)
(require 'dabbrev)

;;; Load the code under test (only function definitions, skip mode activation)
(defvar fate-emacs-dir (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; We define the functions directly to avoid activating fido-vertical-mode in batch
(load (expand-file-name "lisp/completion.el" fate-emacs-dir) nil t)

;;; +fate/dabbrev-capf

(ert-deftest test-fate/dabbrev-capf-returns-nil-no-symbol ()
  "Should return nil when point is not on a symbol."
  (with-temp-buffer
    (insert "  ")
    (goto-char 2)
    (should-not (+fate/dabbrev-capf))))

(ert-deftest test-fate/dabbrev-capf-returns-nil-no-expansions ()
  "Should return nil when dabbrev finds no expansions."
  (with-temp-buffer
    (insert "xyzzy_unique_symbol")
    (goto-char (point-max))
    (let ((result (+fate/dabbrev-capf)))
      (should-not result))))

(ert-deftest test-fate/dabbrev-capf-returns-expansions ()
  "Should return (start end expansions) when dabbrev finds matches."
  (with-temp-buffer
    (insert "hello world\nhello there\nhel")
    (goto-char (point-max))
    (let ((result (+fate/dabbrev-capf)))
      (should result)
      (should (= (length result) 3))
      (should (listp (nth 2 result)))
      ;; Should find "hello" as an expansion
      (should (member "hello" (nth 2 result))))))

;;; provide
(provide 'test-completion)
