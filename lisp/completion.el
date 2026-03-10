;; -*- lexical-binding: t -*-

;; Minibuffer completion: fido-vertical-mode (built-in)
(fido-vertical-mode 1)
(setq icomplete-prospects-height 15)

;; TAB completes, then indents
(setq tab-always-indent 'complete)

;; Redirect in-buffer completion to the minibuffer (same UI as M-x)
(defun +fate/completion-in-region (start end collection &optional predicate)
  "Complete in-region via the minibuffer so fido-vertical-mode handles display."
  (if (minibufferp)
      (completion--in-region start end collection predicate)
    (let* ((initial (buffer-substring-no-properties start end))
           ;; Pre-resolve candidates into a flat list so icomplete
           ;; doesn't choke on programmatic completion tables.
           (candidates (all-completions initial collection predicate)))
      (when candidates
        (let ((chosen (completing-read "Complete: " candidates nil nil initial)))
          (when (and chosen (not (string-empty-p chosen)))
            (delete-region start end)
            (insert chosen)))))))

(setq completion-in-region-function #'+fate/completion-in-region)

;; Custom dabbrev capf (replaces cape-dabbrev)
(require 'dabbrev)

(defun +fate/dabbrev-capf ()
  "Completion-at-point function for dynamic abbreviation."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (dabbrev--reset-global-variables)
      (let ((expansions (dabbrev--find-all-expansions
                         (buffer-substring-no-properties (car bounds) (cdr bounds))
                         nil)))
        (when expansions
          (list (car bounds) (cdr bounds) expansions))))))

(add-hook 'prog-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
(add-hook 'text-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
