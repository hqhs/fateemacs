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

;; The second argument is evil's symbol-vs-word boundary flag, so it must be
;; literally t -- passing the symbol text there degraded to a word search
;; whenever `thing-at-point' returned nil.

(ert-deftest test-fate/search-symbol-forward-calls-evil ()
  "Should delegate to evil-search-word-forward with symbol boundaries."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'evil-search-word-forward)
               (lambda (count symbolp) (setq called-with (list count symbolp)))))
      (+fate/search-symbol-forward 1)
      (should (equal called-with '(1 t))))))

(ert-deftest test-fate/search-symbol-backward-calls-evil ()
  "Should delegate to evil-search-word-backward with symbol boundaries."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'evil-search-word-backward)
               (lambda (count symbolp) (setq called-with (list count symbolp)))))
      (+fate/search-symbol-backward 1)
      (should (equal called-with '(1 t))))))

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

;;; +fate/format-after-save (async formatter)
;; Note: async formatter tests are in test/test-async-format.el

;;; provide
(provide 'test-autoloads)
