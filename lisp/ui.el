;; -*- lexical-binding: t -*-

(setq ring-bell-function 'ignore)
(setq-default display-fill-column-indicator-column 80)
(setq-default display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)

(defvar +fate/original-background nil
  "Stores the original background color.")

(defun +fate/toggle-background-transparency ()
  "Toggle between the original background and a transparent background in terminal Emacs."
  (interactive)
  (unless (display-graphic-p)
    ;; wouldn't work in case theme is switched between toggles, but who cares
    (if +fate/original-background
        (progn
          (set-face-background 'default +fate/original-background)
          (setq +fate/original-background nil))
      (setq +fate/original-background (face-background 'default))
      (set-face-background 'default "rgba:0000/0000/0000/7500"))))


;; (add-hook 'after-init-hook 'set-background-for-terminal)

(defun +fate/shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/"))))
    (let ((path (nreverse
                 (cons (car path)
                       (mapcar #'(lambda (x) (substring x 0 1))
                              (cdr path))))))
      (string-join path "/"))))

(defun +fate/mode-line-directory ()
  "Return directory for mode-line."
  (when default-directory
    (+fate/shorten-directory default-directory 20)))

;; Custom mode-line format
(setq-default mode-line-format
      '(;; Position info
        " %l:%c "
        ;; Buffer status
        (:eval (cond (buffer-read-only "RO ")
                    ((buffer-modified-p) "** ")
                    (t "-- ")))
        ;; Directory and file name
        (:eval (let ((dir (+fate/mode-line-directory))
                     (name (buffer-name)))
                 (if dir
                     (propertize (concat dir "/" name) 'face 'bold)
                   (propertize name 'face 'bold))))
        ;; Project info
        (:eval (when-let ((project (project-current)))
                (format " [%s]" (project-name project))))
        ;; Version control
        (:eval (when-let ((branch (vc-git-mode-line-string buffer-file-name)))
                 (format " (%s)" branch)))
        ;; Major mode
        " %m"
        ;; Evil state
        (:eval (when (bound-and-true-p evil-mode)
                (let ((state (evil-state-property evil-state :tag t)))
                  (when state
                    (format " [%s]" state)))))
        ;; Process status
        " "
        mode-line-process))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-monokai-classic t)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 145
                      :weight 'normal
                      :width 'normal))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package ligature
  :straight t
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
				       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
				       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
				       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
				       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
				       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
				       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
				       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
				       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
				       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
