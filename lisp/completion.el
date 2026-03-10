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
           (completion (completing-read
                        "Complete: " collection predicate nil initial)))
      (when completion
        (delete-region start end)
        (insert completion)))))

(setq completion-in-region-function #'+fate/completion-in-region)

;; Custom dabbrev capf (replaces cape-dabbrev)
(defun +fate/dabbrev-capf ()
  "Completion-at-point function for dynamic abbreviation."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (dabbrev--find-all-expansions
             (buffer-substring-no-properties (car bounds) (cdr bounds))
             nil)))))

(add-hook 'prog-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
(add-hook 'text-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'+fate/dabbrev-capf 20 t)))
