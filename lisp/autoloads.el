;; -*- lexical-binding: t -*-

;;;###autoload
(defun +fate/copy-file-path ()
  "Copy the current buffer's file path to kill ring."
  (interactive)
  (if-let* ((file-path (or (buffer-file-name) default-directory))
            (project-root (project-root (project-current t)))
            (relative-path (file-relative-name file-path project-root)))
      (progn
        (kill-new relative-path)
        (message "Copied: %s" relative-path))
    (message "No file associated with buffer")))

;;;###autoload
(defun +fate/search-project-for-symbol-at-point (symbol)
  "Searches the current project using ripgrep"
  (interactive (list (regexp-quote (thing-at-point 'symbol t))))
  (let ((dir (project-root (project-current t))))
    (consult-ripgrep dir symbol)))

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

(defun +fate/region-active-p ()
  "Check if the region is active."
  (if (boundp 'evil-state)
      (evil-visual-state-p)
    (region-active-p)))

;;;###autoload
(defun +fate/project-search (&optional arg)
  "Search project root with ripgrep. With ARG, include hidden files."
  (interactive "P")
  (+fate--rg-search (project-root (project-current t)) arg))

;;;###autoload
(defun +fate/search-cwd (&optional arg)
  "Search current directory with ripgrep. With ARG, include hidden files."
  (interactive "P")
  (+fate--rg-search default-directory arg))

;;;###autoload
(defun +fate/search-other-cwd (&optional arg)
  "Search from a prompted directory."
  (interactive "P")
  (+fate--rg-search (read-directory-name "Search in: ") arg))

(defun +fate--rg-search (directory &optional include-hidden)
  "Run ripgrep in DIRECTORY. With INCLUDE-HIDDEN, search hidden files."
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (let* ((query (or (when (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                    (read-string "Search: ")))
         (default-directory directory)
         (cmd (format "rg --no-heading --line-number --column --smart-case %s %s"
                      (if include-hidden "--hidden --no-ignore" "")
                      (shell-quote-argument query)))
         (hits (split-string (shell-command-to-string cmd) "\n" t)))
    (xref-show-xrefs
     (mapcar (lambda (hit)
               (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" hit)
                 (xref-make (match-string 4 hit)
                            (xref-make-file-location
                             (expand-file-name (match-string 1 hit) directory)
                             (string-to-number (match-string 2 hit))
                             (string-to-number (match-string 3 hit))))))
             hits)
     nil)))

;;;###autoload
(defun +fate/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include hidden files."
  (interactive "P")
  (let ((default-directory
         (if (eq arg 'other)
             (let ((projects (project-known-project-roots)))
               (if projects
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects")))
           default-directory)))
    (if (eq arg 'other)
        (call-interactively #'+fate/project-search)
      (+fate/project-search arg))))

;;;###autoload
(defun +fate/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+fate/search-project 'other))
