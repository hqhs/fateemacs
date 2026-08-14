;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-marks.el --- ERT tests for +fate session marks

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'bookmark)
(require 'savehist)

;;; Load the code under test. project.el needs fate-cache-dir defined.
(let ((lisp-dir (expand-file-name
                 "lisp/"
                 (file-name-directory
                  (directory-file-name (file-name-directory load-file-name))))))
  (defvar fate-cache-dir (expand-file-name "../cache/" lisp-dir))
  (load (expand-file-name "project.el" lisp-dir)))

;;; Helpers

(defmacro +fate-test--with-project (root &rest body)
  "Run BODY with a fresh marks table and `project-current' pinned to ROOT.
Pinning the project root makes the marks key independent of the current
buffer's `default-directory', which is what `+fate--project-marks-root'
keys on in real use."
  (declare (indent 1))
  `(let ((+fate--project-marks (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _ _dir) (cons 'transient ,root)))
               ((symbol-function 'project-root)
                (lambda (proj) (cdr proj))))
       ,@body)))

(defun +fate-test--make-file (dir name contents)
  "Write CONTENTS to NAME under DIR and return its absolute path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert contents))
    path))

;; Pick the single stored mark's label, for the stubbed `completing-read'.
(defun +fate-test--only-label (root)
  (plist-get (car (gethash root +fate--project-marks)) :label))

;;; +fate/mark-set

(ert-deftest test-fate/mark-set-records-entry ()
  "Setting a mark stores one entry under the project root."
  (let ((dir (make-temp-file "fate-marks" t)))
    (unwind-protect
        (+fate-test--with-project dir
          (let ((f (+fate-test--make-file dir "a.txt" "alpha\nbeta\ngamma\n")))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (progn
                    (goto-char (point-min))
                    (forward-line 2)        ; on "gamma"
                    (+fate/mark-set)
                    (let ((marks (gethash dir +fate--project-marks)))
                      (should (= 1 (length marks)))
                      (should (string-match-p "a\\.txt:3 .*gamma"
                                              (plist-get (car marks) :label)))))
                (kill-buffer)))))
      (delete-directory dir t))))

(ert-deftest test-fate/mark-set-dedups-same-line ()
  "Re-marking the same file+line replaces rather than appends."
  (let ((dir (make-temp-file "fate-marks" t)))
    (unwind-protect
        (+fate-test--with-project dir
          (let ((f (+fate-test--make-file dir "a.txt" "alpha\nbeta\ngamma\n")))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (progn
                    (goto-char (point-min))
                    (forward-line 2)
                    (+fate/mark-set)
                    (+fate/mark-set)        ; same line again
                    (should (= 1 (length (gethash dir +fate--project-marks))))
                    (forward-line -1)       ; different line
                    (+fate/mark-set)
                    (should (= 2 (length (gethash dir +fate--project-marks)))))
                (kill-buffer)))))
      (delete-directory dir t))))

;;; +fate/mark-jump  (the relocation guarantee — Q1)

(ert-deftest test-fate/mark-jump-relocates-after-edit-live ()
  "Editing above a mark in a live buffer; jump still lands on the line."
  (let ((dir (make-temp-file "fate-marks" t)))
    (unwind-protect
        (+fate-test--with-project dir
          (let ((f (+fate-test--make-file dir "a.txt" "alpha\nbeta\ngamma\ndelta\n")))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (progn
                    (goto-char (point-min))
                    (forward-line 2)        ; on "gamma"
                    (+fate/mark-set)
                    ;; insert two lines ABOVE the mark
                    (goto-char (point-min))
                    (insert "NEW1\nNEW2\n")
                    (goto-char (point-max)) ; move point away
                    (cl-letf (((symbol-function 'completing-read)
                               (lambda (&rest _) (+fate-test--only-label dir))))
                      (+fate/mark-jump))
                    (should (string= "gamma"
                                     (string-trim (thing-at-point 'line t)))))
                (kill-buffer)))))
      (delete-directory dir t))))

(ert-deftest test-fate/mark-jump-relocates-after-reopen ()
  "Kill the buffer, edit the file on disk, reopen via jump: still on line.
Uses padding so >=16 chars surround the mark, exercising bookmark's
context-string relocation (the live-marker fast path is gone after the
buffer is killed)."
  (let ((dir (make-temp-file "fate-marks" t))
        ;; >=16 chars before and after the target line ensure context is stored
        (pad-before "aaaaaaaa\nbbbbbbbb\ncccccccc\n")
        (pad-after  "dddddddd\neeeeeeee\nffffffff\n"))
    (unwind-protect
        (+fate-test--with-project dir
          (let* ((body (concat pad-before "TARGETLINE\n" pad-after))
                 (f (+fate-test--make-file dir "a.txt" body))
                 (target-1 (1+ (cl-count ?\n pad-before)))) ; line # of TARGETLINE
            (with-current-buffer (find-file-noselect f)
              (goto-char (point-min))
              (forward-line (1- target-1))  ; on "TARGETLINE"
              (should (string= "TARGETLINE" (string-trim (thing-at-point 'line t))))
              (+fate/mark-set)
              (kill-buffer))
            ;; edit the file on disk while no buffer is open: prepend lines
            (+fate-test--make-file dir "a.txt"
                                   (concat "NEW1\nNEW2\n" body))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (+fate-test--only-label dir))))
              (+fate/mark-jump))
            (unwind-protect
                (should (string= "TARGETLINE"
                                 (string-trim (thing-at-point 'line t))))
              (when (get-file-buffer f) (kill-buffer (get-file-buffer f))))))
      (delete-directory dir t))))

;;; +fate/mark-jump  error path

(ert-deftest test-fate/mark-jump-no-marks-errors ()
  "Jumping with no marks in the project signals a user-error."
  (let ((dir (make-temp-file "fate-marks" t)))
    (unwind-protect
        (+fate-test--with-project dir
          (should-error (+fate/mark-jump) :type 'user-error))
      (delete-directory dir t))))

;;; +fate/mark-clear

(ert-deftest test-fate/mark-clear-empties-project ()
  "Clearing forgets all marks for the current project only."
  (let ((dir (make-temp-file "fate-marks" t)))
    (unwind-protect
        (+fate-test--with-project dir
          (let ((f (+fate-test--make-file dir "a.txt" "alpha\nbeta\n")))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (progn
                    (+fate/mark-set)
                    (should (= 1 (length (gethash dir +fate--project-marks))))
                    (+fate/mark-clear)
                    (should (null (gethash dir +fate--project-marks))))
                (kill-buffer)))))
      (delete-directory dir t))))

(provide 'test-marks)
;;; test-marks.el ends here
