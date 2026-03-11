;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-async-format.el --- ERT tests for async format-on-save

(require 'ert)

;;; Load the code under test
(let* ((test-dir (file-name-directory load-file-name))
       (root-dir (file-name-directory (directory-file-name test-dir)))
       (lisp-dir (expand-file-name "lisp/" root-dir)))
  (defvar fate-cache-dir (expand-file-name "cache/" root-dir))
  (load (expand-file-name "prog-conf.el" lisp-dir) nil t))

;;; Helper

(defun +fate-test--wait-for-process (buf &optional timeout)
  "Wait up to TIMEOUT seconds (default 5) for format process in BUF to finish.
Keeps draining output after process exits to ensure sentinel has run."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (with-current-buffer buf
      (while (and +fate--format-process
                  (process-live-p +fate--format-process)
                  (< (float-time) deadline))
        (accept-process-output +fate--format-process 0.1))
      ;; Process is dead, but sentinel may not have run yet.
      ;; Drain remaining output to trigger it.
      (when +fate--format-process
        (accept-process-output +fate--format-process 0.1)))))

;;; Tests

(ert-deftest test-fate/format-noop-when-no-command ()
  "Should do nothing when +fate-format-command is nil."
  (with-temp-buffer
    (insert "hello  ")
    (let ((+fate-format-command nil)
          (+fate--format-after-save-in-progress nil))
      (+fate/format-after-save)
      (should (string= (buffer-string) "hello  ")))))

(ert-deftest test-fate/format-async-applies-formatter ()
  "Should asynchronously format buffer contents."
  (let* ((tmp (make-temp-file "fate-fmt-test"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello world")
          (setq-local +fate-format-command '("tr" "a-z" "A-Z"))
          (let ((+fate--format-after-save-in-progress nil))
            (+fate/format-after-save)
            (+fate-test--wait-for-process buf)
            (should (string= (buffer-string) "HELLO WORLD"))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmp))))

(ert-deftest test-fate/format-async-skips-on-buffer-change ()
  "Should skip applying format if buffer changed during formatting."
  (let* ((tmp (make-temp-file "fate-fmt-test"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello world")
          (setq-local +fate-format-command '("tr" "a-z" "A-Z"))
          (let ((+fate--format-after-save-in-progress nil))
            (+fate/format-after-save)
            ;; Simulate user editing during format
            (goto-char (point-max))
            (insert " extra")
            (+fate-test--wait-for-process buf)
            ;; Buffer should keep the user's edit, not the formatted version
            (should (string= (buffer-string) "hello world extra"))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmp))))

(ert-deftest test-fate/format-async-no-loop ()
  "The re-save after format should not spawn a second formatter process."
  (let* ((tmp (make-temp-file "fate-fmt-test"))
         (buf (find-file-noselect tmp))
         (process-count 0))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello")
          (setq-local +fate-format-command '("cat"))
          ;; Count actual make-process calls (not just function entry)
          (advice-add 'make-process :before
                      (lambda (&rest _) (cl-incf process-count))
                      '((name . test-counter)))
          (let ((+fate--format-after-save-in-progress nil))
            (+fate/format-after-save)
            (+fate-test--wait-for-process buf)
            ;; Only one process should have been spawned
            (should (= process-count 1))))
      (advice-remove 'make-process 'test-counter)
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmp))))

(ert-deftest test-fate/format-async-kills-previous-process ()
  "Starting a new format should kill any in-flight process."
  (let* ((tmp (make-temp-file "fate-fmt-test"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello")
          ;; Use sleep to simulate a slow formatter
          (setq-local +fate-format-command '("sleep" "10"))
          (let ((+fate--format-after-save-in-progress nil))
            (+fate/format-after-save)
            (let ((first-proc +fate--format-process))
              (should (process-live-p first-proc))
              ;; Start a second format — should kill the first
              (setq-local +fate-format-command '("cat"))
              (+fate/format-after-save)
              (should-not (process-live-p first-proc))
              (+fate-test--wait-for-process buf))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmp))))

(ert-deftest test-fate/format-async-handles-failure ()
  "Should not modify buffer when formatter exits non-zero."
  (let* ((tmp (make-temp-file "fate-fmt-test"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "hello world")
          (setq-local +fate-format-command '("false"))
          (let ((+fate--format-after-save-in-progress nil))
            (+fate/format-after-save)
            (+fate-test--wait-for-process buf)
            (should (string= (buffer-string) "hello world"))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmp))))

(ert-deftest test-fate/buffer-hash-changes ()
  "Buffer hash should change when content changes."
  (with-temp-buffer
    (insert "hello")
    (let ((hash1 (+fate--buffer-hash)))
      (insert " world")
      (should-not (equal hash1 (+fate--buffer-hash))))))

(ert-deftest test-fate/buffer-hash-stable ()
  "Buffer hash should be stable for same content."
  (with-temp-buffer
    (insert "hello")
    (should (equal (+fate--buffer-hash) (+fate--buffer-hash)))))

(provide 'test-async-format)
