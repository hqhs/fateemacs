;; -*- lexical-binding: t -*-
;;
;; One jump list for every kind of navigation.
;;
;; Evil records a jump only when `pre-command-hook' sees a `this-command' that
;; carries the `:jump' property (see `evil--jump-hook'), plus a buffer-crossing
;; fallback on `post-command-hook'. That covers evil's own motions and
;; `xref-find-definitions', and nothing else we navigate with:
;;
;; - A command that wraps a jump in a plain `defun' -- `+fate/tags-find-definition',
;;   `+fate/mark-jump' -- reports its own symbol as `this-command', which has no
;;   `:jump' property, so nothing is recorded.
;; - Jumps that stay inside one buffer (imenu, occur) never trip the
;;   buffer-crossing fallback either, so `C-o' has nowhere to go back to.
;; - `xref-go-back' pops xref's own marker stack, a second history that drifts
;;   out of sync with `C-o'.
;;
;; The remedy is Doom's (modules/doom/compat/+better-jumper.el): advise each
;; navigation command to push the origin explicitly, and route xref's stack
;; through evil's ring so the two can't disagree. Doom drives `better-jumper'
;; here; evil's own ring already does per-window storage, copy-on-split and
;; savehist persistence, so we drive `evil-set-jump' instead and skip the
;; dependency.

(defun +fate/set-jump-a (fn &rest args)
  "Push a jump point, then apply FN to ARGS without recording further jumps.
If ARGS starts with a marker, that position is recorded instead of point."
  (evil-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t))
    (apply fn args)))

(defun +fate/set-jump-maybe-a (fn &rest args)
  "Apply FN to ARGS, pushing a jump point only if point actually moved.
For commands that may not land anywhere -- a lookup that finds nothing, a
prompt that is aborted -- so they leave no junk in the jump list."
  (let* ((origin (point-marker))
         (result (let ((evil--jumps-jumping t))
                   (apply fn args)))
         (dest (point-marker)))
    (unless (equal origin dest)
      (with-current-buffer (marker-buffer origin)
        (evil-set-jump (if (markerp (car args)) (car args) origin))))
    (set-marker origin nil)
    (set-marker dest nil)
    result))

(defun +fate/set-jump-h ()
  "Push a jump point. Always returns nil, for hooks that short-circuit."
  (when (get-buffer-window)
    (evil-set-jump))
  nil)

(with-eval-after-load 'evil
  ;; `xref-find-definitions' and `xref-find-references' already carry `:jump'
  ;; (see `evil-integration.el'), so they are absent from these lists.

  ;; Always lands somewhere, or signals.
  (dolist (fn '(imenu
                outline-up-heading))
    (advice-add fn :around #'+fate/set-jump-a))

  ;; May not move point: aborted prompts, lookups that find nothing.
  (dolist (fn '(+fate/tags-find-definition
                +fate/mark-jump
                bookmark-jump
                xref-find-apropos
                project-find-file
                compile-goto-error
                occur-mode-goto-occurrence))
    (advice-add fn :around #'+fate/set-jump-maybe-a))

  ;; Killing a file buffer becomes undoable with `C-o'. Advising `kill-buffer'
  ;; would catch programmatic kills too, which we don't want.
  (add-hook 'kill-buffer-hook #'+fate/set-jump-h)

  ;; Collapse xref's marker stack into the jump list, so `M-,' and `C-o' are
  ;; the same history read from the same end.
  (global-set-key [remap xref-go-back] #'evil-jump-backward)
  (global-set-key [remap xref-go-forward] #'evil-jump-forward)
  (global-set-key [remap xref-pop-marker-stack] #'evil-jump-backward))

(provide 'jumps)
