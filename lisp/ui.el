;; -*- lexical-binding: t -*-

(setq ring-bell-function 'ignore)
(setq-default display-fill-column-indicator-column 100)
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

;; Mode-line is configured in lisp/modeline.el


(use-package hl-line
  :config
  (global-hl-line-mode 1))

;; Fira Code ligatures via built-in HarfBuzz (Emacs 28+, no external dep)
(when (and (fboundp 'set-fontset-font) (>= emacs-major-version 28))
  (let ((ligatures '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                     "{-" "::" ":::" ":=" "!!" "!=" "!==" "-}" "----"
                     "-->" "->" "->>" "-<" "-<<" "-~" "#{" "#[" "##"
                     "###" "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".."
                     "..<" "..." "?=" "??" "/*" "/**" "/=" "/==" "/>"
                     "&&" "||" "||=" "|=" "|>" "^=" "$>" "++" "+++"
                     "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>="
                     ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<!--"
                     "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>"
                     "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~~"
                     "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))
    (dolist (pat ligatures)
      (set-char-table-range
       composition-function-table
       (aref pat 0)
       (nconc (char-table-range composition-function-table (aref pat 0))
              (list (vector (regexp-quote pat) 0 'compose-gstring-for-graphic)))))))

;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 180
                    :weight 'normal
                    :width  'normal)

;; Theme: checked-in monokai (no external dep)
(load-theme 'monokai t)

