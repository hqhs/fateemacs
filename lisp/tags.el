;; -*- lexical-binding: t -*-

;; Universal Ctags + built-in etags for tags-based navigation.
;; Works alongside eglot by bypassing xref backend dispatch.

(require 'etags)

(defvar +fate-ctags-program "ctags"
  "Path to the Universal Ctags binary.")

(defvar +fate-ctags-extra-args '("--extras=+q" "--fields=+i")
  "Extra arguments passed to ctags when generating tags.")

(defun +fate/tags-find-definition ()
  "Find tag definition using etags, bypassing eglot's xref backend."
  (interactive)
  (let ((xref-backend-functions '(etags--xref-backend)))
    (call-interactively #'xref-find-definitions)))

(defun +fate/tags-generate ()
  "Regenerate TAGS file for the current project using Universal Ctags."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (cmd (string-join
               (append (list +fate-ctags-program "-Re" "-f" "TAGS")
                       +fate-ctags-extra-args
                       (list "."))
               " ")))
    (message "Generating tags in %s..." root)
    (set-process-sentinel
     (start-process-shell-command "ctags" "*ctags*" cmd)
     (lambda (proc _event)
       (if (zerop (process-exit-status proc))
           (progn
             (visit-tags-table (expand-file-name "TAGS" root))
             (message "Tags generated in %s" root))
         (message "ctags failed (exit %d)" (process-exit-status proc)))))))

(provide 'tags-config)
