;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-project-compile.el --- ERT tests for +fate/project-compile

(require 'ert)
(require 'project)
(require 'savehist)

;;; Load the code under test
(let* ((test-dir (file-name-directory load-file-name))
       (root-dir (file-name-directory (directory-file-name test-dir)))
       (lisp-dir (expand-file-name "lisp/" root-dir)))
  (defvar fate-cache-dir (expand-file-name "cache/" root-dir))
  (load (expand-file-name "project.el" lisp-dir) nil t))

(ert-deftest test-fate/project-compile-stores-command ()
  "Should store compile command in per-project hash table."
  (let ((+fate--project-compile-commands (make-hash-table :test 'equal))
        (fake-root "/tmp/project-a/"))
    (cl-letf (((symbol-function 'project-current) (lambda (_) (cons 'vc fake-root)))
              ((symbol-function 'project-root) (lambda (_) fake-root))
              ((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (setq compile-command command)))
              ;; Make call-interactively just call compile with compile-command
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _)
                 (funcall fn compile-command))))
      (let ((compile-command "make test"))
        (+fate/project-compile)
        (should (equal (gethash fake-root +fate--project-compile-commands)
                       "make test"))))))

(ert-deftest test-fate/project-compile-restores-saved-command ()
  "Should use previously saved command for the same project."
  (let ((+fate--project-compile-commands (make-hash-table :test 'equal))
        (fake-root "/tmp/project-b/")
        (received-command nil))
    (puthash fake-root "cargo build" +fate--project-compile-commands)
    (cl-letf (((symbol-function 'project-current) (lambda (_) (cons 'vc fake-root)))
              ((symbol-function 'project-root) (lambda (_) fake-root))
              ((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (setq received-command command)
                 (setq compile-command command)))
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _)
                 (funcall fn compile-command))))
      (let ((compile-command "make -k"))
        (+fate/project-compile)
        (should (equal received-command "cargo build"))))))

(ert-deftest test-fate/project-compile-isolates-projects ()
  "Different projects should have independent compile commands."
  (let ((+fate--project-compile-commands (make-hash-table :test 'equal))
        (root-a "/tmp/project-a/")
        (root-b "/tmp/project-b/"))
    (puthash root-a "make" +fate--project-compile-commands)
    (puthash root-b "npm run build" +fate--project-compile-commands)
    (should (equal (gethash root-a +fate--project-compile-commands) "make"))
    (should (equal (gethash root-b +fate--project-compile-commands) "npm run build"))))

(provide 'test-project-compile)
