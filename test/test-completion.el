;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-completion.el --- ERT tests for fate completion functions

(require 'ert)
(require 'dabbrev)

;;; Load the code under test (only function definitions, skip mode activation)
(defvar fate-emacs-dir (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; We define the functions directly to avoid activating fido-vertical-mode in batch
(load (expand-file-name "lisp/completion.el" fate-emacs-dir) nil t)

;;; +fate/completion-in-region

(ert-deftest test-fate/completion-in-region-inserts-chosen ()
  "Should replace region with the chosen completion."
  (with-temp-buffer
    (insert "hel")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _) (car coll))))
      (+fate/completion-in-region 1 4 '("hello" "help" "helm"))
      (should (string= (buffer-string) "hello")))))

(ert-deftest test-fate/completion-in-region-no-candidates ()
  "Should do nothing when collection yields no matches."
  (with-temp-buffer
    (insert "xyz")
    (let ((cr-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq cr-called t) nil)))
        (+fate/completion-in-region 1 4 '("alpha" "beta"))
        ;; "xyz" has no match via all-completions, so completing-read should never be called
        (should-not cr-called)
        (should (string= (buffer-string) "xyz"))))))

(ert-deftest test-fate/completion-in-region-nil-collection ()
  "Should handle nil collection gracefully."
  (with-temp-buffer
    (insert "foo")
    (+fate/completion-in-region 1 4 nil)
    (should (string= (buffer-string) "foo"))))

(ert-deftest test-fate/completion-in-region-function-table ()
  "Should work with a programmatic completion table (function)."
  (with-temp-buffer
    (insert "he")
    (let ((table (lambda (string pred action)
                   (complete-with-action action '("hello" "help" "world") string pred))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (car coll))))
        (+fate/completion-in-region 1 3 table)
        (should (string= (buffer-string) "hello"))))))

(ert-deftest test-fate/completion-in-region-in-minibuffer ()
  "Should delegate to completion--in-region when in minibuffer."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'minibufferp) (lambda () t))
              ((symbol-function 'completion--in-region)
               (lambda (s e c p) (setq delegated (list s e c p)))))
      (+fate/completion-in-region 1 5 '("a" "b") #'identity)
      (should (equal delegated (list 1 5 '("a" "b") #'identity))))))

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
