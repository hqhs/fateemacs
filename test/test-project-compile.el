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

;;; Test / run share the machinery but must not share the storage

(defvar +test-offered nil
  "The command `compile' was last invoked with.")

(defmacro +test-with-fake-project (root &rest body)
  "Run BODY with `compile' stubbed and the project root forced to ROOT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'project-current) (lambda (_) (cons 'vc ,root)))
             ((symbol-function 'project-root) (lambda (_) ,root))
             ((symbol-function 'compile)
              (lambda (command &optional _comint)
                (setq +test-offered command
                      compile-command command))))
     ,@body))

(defmacro +test-typing (typed &rest body)
  "Run BODY as if the user typed TYPED at the compile prompt.
With TYPED nil, accept whatever default the prompt was seeded with."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'call-interactively)
              (lambda (fn &rest _) (funcall fn (or ,typed compile-command)))))
     ,@body))

(ert-deftest test-fate/project-test-and-run-store-separately ()
  "Compile, test and run keep independent commands for the same project."
  (let ((+fate--project-compile-commands (make-hash-table :test 'equal))
        (+fate--project-test-commands (make-hash-table :test 'equal))
        (+fate--project-run-commands (make-hash-table :test 'equal))
        (root "/tmp/project-c/"))
    (+test-with-fake-project root
      (+test-typing "cargo build" (+fate/project-compile))
      (+test-typing "cargo test"  (+fate/project-test))
      (+test-typing "cargo run"   (+fate/project-run)))
    (should (equal (gethash root +fate--project-compile-commands) "cargo build"))
    (should (equal (gethash root +fate--project-test-commands) "cargo test"))
    (should (equal (gethash root +fate--project-run-commands) "cargo run"))))

(ert-deftest test-fate/project-test-restores-saved-command ()
  "A second invocation seeds the prompt with the remembered command."
  (let ((+fate--project-test-commands (make-hash-table :test 'equal))
        (root "/tmp/project-d/"))
    (puthash root "pytest -x" +fate--project-test-commands)
    (+test-with-fake-project root
      ;; A stale global `compile-command' must not leak into the test slot.
      (let ((compile-command "make -k"))
        (+test-typing nil (+fate/project-test))))
    (should (equal +test-offered "pytest -x"))))

(ert-deftest test-fate/project-test-defaults-to-empty ()
  "With nothing remembered, test/run prompt empty rather than inheriting
`compile-command' -- \"make -k \" is a build command, not a test command."
  (let ((+fate--project-run-commands (make-hash-table :test 'equal))
        (root "/tmp/project-e/"))
    (+test-with-fake-project root
      (let ((compile-command "make -k "))
        (+test-typing nil (+fate/project-run))))
    (should (equal +test-offered ""))))

(ert-deftest test-fate/project-run-isolates-projects ()
  "The same slot in two projects keeps two commands."
  (let ((+fate--project-run-commands (make-hash-table :test 'equal)))
    (+test-with-fake-project "/tmp/project-f/"
      (+test-typing "./build/game" (+fate/project-run)))
    (+test-with-fake-project "/tmp/project-g/"
      (+test-typing "npm start" (+fate/project-run)))
    (should (equal (gethash "/tmp/project-f/" +fate--project-run-commands)
                   "./build/game"))
    (should (equal (gethash "/tmp/project-g/" +fate--project-run-commands)
                   "npm start"))))

(ert-deftest test-fate/project-commands-are-persisted ()
  "All three tables ride along in savehist."
  (dolist (var '(+fate--project-compile-commands
                 +fate--project-test-commands
                 +fate--project-run-commands))
    (should (memq var savehist-additional-variables))))

(provide 'test-project-compile)
