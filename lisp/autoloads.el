;; -*- lexical-binding: t -*-

;;;###autoload
(defun +fate/search-project-for-symbol-at-point (symbol)
  "Searches the current project (or directory if there's none) using ripgrep"
  (interactive (list (rxt-quote-pcre (thing-at-point 'symbol t))))
  (consult-ripgrep (projectile-project-root nil) symbol))

;;;###autoload
(defun +fate/search-symbol-forward (count symbol)
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (thing-at-point 'symbol t)))
  (evil-search-word-forward count symbol))

;;;###autoload
(defun +fate/search-symbol-backward (count symbol)
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (thing-at-point 'symbol t)))
  (evil-search-word-backward count symbol))
