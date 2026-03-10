;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-autoloads.el --- ERT tests for fate autoload functions

(require 'ert)
(require 'project)

;;; Load the code under test
(let ((lisp-dir (expand-file-name "lisp/" (file-name-directory (directory-file-name (file-name-directory load-file-name))))))
  (load (expand-file-name "autoloads.el" lisp-dir))
  (load (expand-file-name "prog-conf.el" lisp-dir) nil t))

;;; +fate/copy-file-path

(ert-deftest test-fate/copy-file-path-with-file ()
  "Should copy relative file path to kill ring."
  (let ((temp-dir (make-temp-file "fate-test" t)))
    (unwind-protect
        (let* ((file (expand-file-name "src/foo.el" temp-dir))
               (default-directory temp-dir))
          (make-directory (file-name-directory file) t)
          (write-region "" nil file)
          ;; Simulate a VC-backed project
          (make-directory (expand-file-name ".git" temp-dir) t)
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (+fate/copy-file-path)
                  (should (string= (current-kill 0) "src/foo.el")))
              (kill-buffer))))
      (delete-directory temp-dir t))))

;;; +fate/search-symbol-forward / backward

(ert-deftest test-fate/search-symbol-forward-calls-evil ()
  "Should delegate to evil-search-word-forward."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'evil-search-word-forward)
               (lambda (count sym) (setq called-with (list count sym)))))
      (+fate/search-symbol-forward 1 "foo")
      (should (equal called-with '(1 "foo"))))))

(ert-deftest test-fate/search-symbol-backward-calls-evil ()
  "Should delegate to evil-search-word-backward."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'evil-search-word-backward)
               (lambda (count sym) (setq called-with (list count sym)))))
      (+fate/search-symbol-backward 1 "bar")
      (should (equal called-with '(1 "bar"))))))

;;; +fate/region-active-p

(ert-deftest test-fate/region-active-p-no-evil ()
  "Without evil, should fall back to region-active-p."
  (let ((evil-state nil))
    (makunbound 'evil-state)
    (should-not (+fate/region-active-p))))

;;; +fate--rg-search

(ert-deftest test-fate--rg-search-errors-without-rg ()
  "Should signal error when ripgrep is not found."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (+fate--rg-search "/tmp" nil "test")
                  :type 'user-error)))

;;; +fate/format-buffer-on-save

(ert-deftest test-fate/format-buffer-noop-when-no-command ()
  "Should do nothing when +fate-format-command is nil."
  (with-temp-buffer
    (insert "hello  ")
    (let ((+fate-format-command nil))
      (+fate/format-buffer-on-save)
      (should (string= (buffer-string) "hello  ")))))

(ert-deftest test-fate/format-buffer-applies-formatter ()
  "Should pipe buffer through formatter and replace contents."
  (with-temp-buffer
    (insert "hello world")
    ;; Use 'tr' to uppercase as a simple test formatter
    (let ((+fate-format-command '("tr" "a-z" "A-Z")))
      (+fate/format-buffer-on-save)
      (should (string= (buffer-string) "HELLO WORLD")))))

;;; provide
(provide 'test-autoloads)
