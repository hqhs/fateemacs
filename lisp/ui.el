;; -*- lexical-binding: t -*-

(setq ring-bell-function 'ignore)
(setq-default display-fill-column-indicator-column 100)
;; Line numbers: enabled per-buffer from `prog-mode-hook' in prog-conf.el (not
;; globally -- dired/magit/help buffers do not want them), and configured
;; there too, so there is only one place that sets them.

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


;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 160
                    :weight 'normal
                    :width  'normal)

;; A little air between lines
(setq-default line-spacing 0.1)

;; Themes: checked-in monokai (lisp/monokai-theme.el) plus vendored solarized,
;; registered so `load-theme' can reach it. The active one matches nvimrc's
;; `colorscheme monokai'.
(add-to-list 'custom-theme-load-path
             (expand-file-name "solarized-theme/" fate-vendor-dir))
(load-theme 'monokai t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;
;; Two halves that share one engine:
;;
;; - Trackpad/wheel: ultra-scroll replaces `pixel-scroll-precision-mode's wheel
;;   handler (it remaps `pixel-scroll-precision' in that mode's own keymap) and
;;   scrolls past images and tall lines without the stutter the built-in has.
;;   It enables `pixel-scroll-precision-mode' itself, so nothing here does.
;;
;; - Keyboard: animates instead of teleporting, the way neoscroll.nvim does.
;;   `pixel-scroll-precision-interpolate-page' covers C-v / M-v; the commands
;;   below reuse the same interpolation for evil's half-page and `z'-prefixed
;;   motions, which neither mode touches. Bound in keybindings.el.
;;
;; `scroll-margin' 3 and `scroll-conservatively' 101 (prog-conf.el) are left
;; alone despite ultra-scroll's README asking for 0: it saves both, forces
;; them for the duration of a scroll gesture, and restores them on an idle
;; timer, so a nonzero margin costs nothing -- and `zt'/`zb' below need it.

(use-package ultra-scroll
  :init
  ;; Governs the keyboard commands only. ultra-scroll pulls in pixel-scroll.
  (setq pixel-scroll-precision-interpolate-page t
        ;; Long enough to read as motion, short enough not to feel like a wait.
        pixel-scroll-precision-interpolation-total-time 0.12)
  :config
  (ultra-scroll-mode 1))

(defun +fate--window-row ()
  "Return point's screen row in the selected window, 0 being the top row.
Counts display lines, so it stays right when lines wrap."
  ;; `window-start' is nil until the window has been displayed once.
  (save-excursion
    (let ((start (or (window-start) (point-min)))
          (row 0))
      (while (> (point) start)
        (vertical-motion -1)
        (setq row (1+ row)))
      row)))

(defun +fate--scroll-animate (lines)
  "Animate scrolling the selected window by LINES display lines.
Positive LINES moves the view down the buffer, negative up.
`pixel-scroll-precision-interpolate' takes pixels and uses the opposite
sign; FACTOR of 1 means \"exactly this far\", as the built-in page
commands pass."
  (pixel-scroll-precision-interpolate
   (* (- lines) (default-line-height)) nil 1))

(defun +fate--scroll-half (direction)
  "Scroll half a window, animated. DIRECTION is 1 for down, -1 for up.
Point keeps the screen row it was on, as Vim's C-d / C-u do. The
interpolation clamps point into the window as it animates, so point is
recomputed from the final `window-start' rather than moved up front."
  (if (not (bound-and-true-p pixel-scroll-precision-mode))
      (if (> direction 0) (evil-scroll-down nil) (evil-scroll-up nil))
    (let ((row (+fate--window-row))
          (lines (max 1 (/ (window-body-height) 2))))
      (+fate--scroll-animate (* direction lines))
      (goto-char (or (window-start) (point)))
      (vertical-motion row))))

(defun +fate/scroll-down-half ()
  "Scroll down half a window, animated."
  (interactive)
  (+fate--scroll-half 1))

(defun +fate/scroll-up-half ()
  "Scroll up half a window, animated."
  (interactive)
  (+fate--scroll-half -1))

(defun +fate--scroll-point-to-row (target)
  "Animate scrolling until point sits on window row TARGET."
  (if (not (bound-and-true-p pixel-scroll-precision-mode))
      (recenter target)
    (+fate--scroll-animate (- (+fate--window-row) target))))

(defun +fate/scroll-line-to-center ()
  "Animated `zz': centre the line point is on."
  (interactive)
  (+fate--scroll-point-to-row (/ (window-body-height) 2)))

(defun +fate/scroll-line-to-top ()
  "Animated `zt': bring the line point is on to the top, honouring `scroll-margin'."
  (interactive)
  (+fate--scroll-point-to-row scroll-margin))

(defun +fate/scroll-line-to-bottom ()
  "Animated `zb': bring the line point is on to the bottom, honouring `scroll-margin'."
  (interactive)
  (+fate--scroll-point-to-row (- (window-body-height) scroll-margin 1)))
