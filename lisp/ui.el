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

;; Custom mode-line format
(setq-default mode-line-format
              '((:eval evil-mode-line-tag)  ; Add Evil state
                 "%e"
                 mode-line-front-space
                 mode-line-mule-info
                 mode-line-client
                 mode-line-modified
                 mode-line-remote
                 mode-line-frame-identification
                 mode-line-buffer-identification
                 "  "
                 (:eval (format-mode-line "%l:%c"))  ; Shows only line:column
                 (vc-mode vc-mode)
                 "  "
                 "  "
                 (:propertize mode-name) ; This shows only the major mode name
                 mode-line-misc-info
                 mode-line-end-spaces))


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

(use-package doom-themes
  :config
  ;; theme, font
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 180
                      :weight 'normal
                      :width  'normal)
  ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-pro t)
  ;; (load-theme 'monokai t) ;; checked-in to the repo

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

