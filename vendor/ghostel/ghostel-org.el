;;; ghostel-org.el --- Org links to ghostel terminals -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Registers the `ghostel:' Org link type.  A link names a directory
;; and optionally a buffer:
;;
;;    ghostel:~/src/project
;;    ghostel:~/src/project::*ghostel*
;;
;; Following the first pops to a live ghostel terminal whose working
;; directory is that directory, or starts a fresh shell there when none
;; exists.  The second behaves like a ghostel bookmark: it reuses the
;; named buffer, typing a `cd' when it sits elsewhere, and otherwise
;; starts that buffer in the directory.  `org-store-link' in a ghostel
;; buffer stores the second form; `org-insert-link' completes `ghostel:'
;; with a directory prompt.
;;
;; Enable it with
;;
;;    (with-eval-after-load 'org (require 'ghostel-org))

;;; Code:

(require 'compat)
(require 'ol)
(require 'ghostel)
(require 'ghostel-bookmark)

(defun ghostel-org-store-link (&optional _interactive)
  "Store a `ghostel:DIR::NAME' link to the current ghostel buffer."
  (when (derived-mode-p 'ghostel-mode)
    (org-link-store-props
     :type "ghostel"
     :link (format "ghostel:%s::%s"
                   (abbreviate-file-name default-directory) (buffer-name))
     :description (buffer-name))))

(defun ghostel-org-open (path _arg)
  "Pop to the ghostel terminal PATH names, creating it when needed.
PATH is DIR (any live terminal in DIR) or DIR::NAME (that buffer,
restored as by `ghostel-bookmark-handler')."
  (pcase-let* ((`(,dir ,name)
                (if (string-match "\\`\\(.*?\\)::\\(.*\\)\\'" path)
                    (list (match-string 1 path) (match-string 2 path))
                  (list path)))
               (dir (file-name-as-directory (expand-file-name dir)))
               (action (append display-buffer--same-window-action
                               '((category . comint)))))
    (if name
        (pop-to-buffer
         (ghostel-bookmark-handler
          `(nil (location . ,dir) (buf-name . ,name)))
         action)
      (if-let* ((buf (seq-find (lambda (b)
                                 (with-current-buffer b
                                   (and (derived-mode-p 'ghostel-mode)
                                        (eq (alist-get 'kind ghostel-identity)
                                            'term)
                                        (process-live-p ghostel--process)
                                        (file-equal-p default-directory dir))))
                               (buffer-list))))
          (pop-to-buffer buf action)
        (let ((default-directory dir))
          (ghostel-create nil action))))))

(defun ghostel-org-complete-link (&optional _arg)
  "Complete a `ghostel:' link by prompting for a directory."
  (concat "ghostel:"
          (abbreviate-file-name (read-directory-name "Ghostel directory: "))))

(org-link-set-parameters "ghostel"
                         :store #'ghostel-org-store-link
                         :follow #'ghostel-org-open
                         :complete #'ghostel-org-complete-link)

(provide 'ghostel-org)
;;; ghostel-org.el ends here
