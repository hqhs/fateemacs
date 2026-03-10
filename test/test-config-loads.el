;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; test/test-config-loads.el --- ERT tests: verify config files parse without errors

(require 'ert)

(defvar +test-emacs-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(ert-deftest test-early-init-parses ()
  "early-init.el should be valid elisp."
  (should (with-temp-buffer
            (insert-file-contents (expand-file-name "early-init.el" +test-emacs-dir))
            (condition-case nil
                (progn (while t (read (current-buffer))) t)
              (end-of-file t)
              (error nil)))))

(ert-deftest test-init-parses ()
  "init.el should be valid elisp."
  (should (with-temp-buffer
            (insert-file-contents (expand-file-name "init.el" +test-emacs-dir))
            (condition-case nil
                (progn (while t (read (current-buffer))) t)
              (end-of-file t)
              (error nil)))))

(ert-deftest test-all-lisp-files-parse ()
  "All .el files in lisp/ should be valid elisp."
  (let ((lisp-dir (expand-file-name "lisp/" +test-emacs-dir)))
    (dolist (file (directory-files lisp-dir t "\\.el\\'"))
      (should (with-temp-buffer
                (insert-file-contents file)
                (condition-case err
                    (progn (while t (read (current-buffer))) t)
                  (end-of-file t)
                  (error (message "Parse error in %s: %s" file err) nil)))))))

(ert-deftest test-all-lang-files-parse ()
  "All .el files in lang/ should be valid elisp."
  (let ((lang-dir (expand-file-name "lang/" +test-emacs-dir)))
    (dolist (file (directory-files lang-dir t "\\.el\\'"))
      (should (with-temp-buffer
                (insert-file-contents file)
                (condition-case err
                    (progn (while t (read (current-buffer))) t)
                  (end-of-file t)
                  (error (message "Parse error in %s: %s" file err) nil)))))))

(ert-deftest test-transient-hook-macro-defined ()
  "The transient hook macro should be available after loading init."
  (should (with-temp-buffer
            (insert-file-contents (expand-file-name "init.el" +test-emacs-dir))
            (let ((found nil))
              (condition-case nil
                  (while t
                    (let ((form (read (current-buffer))))
                      (when (and (listp form)
                                 (eq (car form) 'defmacro)
                                 (eq (cadr form) '+fate/add-transient-hook!))
                        (setq found t))))
                (end-of-file nil))
              found))))

(ert-deftest test-cache-dir-defined-in-init ()
  "fate-cache-dir should be defined in init.el."
  (should (with-temp-buffer
            (insert-file-contents (expand-file-name "init.el" +test-emacs-dir))
            (search-forward "fate-cache-dir" nil t))))

(provide 'test-config-loads)
