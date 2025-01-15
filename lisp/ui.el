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

;; Custom mode-line format
(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        "  "
        mode-line-position
        (vc-mode vc-mode)
        "  "
        "  "
        (:propertize mode-name) ; This shows only the major mode name
        mode-line-misc-info
        mode-line-end-spaces))

;; theme, font
(load-theme 'monokai t)
(set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 145
                      :weight 'normal
                      :width 'normal)

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
				       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??"
               ;; ";;"
               "/*" "/**"
				       "/=" "/==" "/>"
               ;; "//" "///"
               "&&" "||" "||=" "|=" "|>" "^=" "$>"
				       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
				       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
				       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
				       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
				       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
