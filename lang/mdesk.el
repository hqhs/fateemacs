;; -*- lexical-binding: t -*-

(defun mdesk-mode ()
  "Major mode for editing .mdesk files."
  (interactive)
  (c-mode)
  (setq mode-name "MDesk")
  (font-lock-add-keywords nil
   '(("@\\(table\\|gen\\|expand\\|c_file\\)" . font-lock-keyword-face)
     ("\\${\\([^}]+\\)}" . font-lock-variable-name-face))))

;; FIXME(hqhs): enabled all hooks for C mode, which is undesired
;; (add-to-list 'auto-mode-alist '("\\.mdesk\\'" . mdesk-mode))
