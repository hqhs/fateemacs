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
  (interactive (list (rxt-quote-pcre (thing-at-point 'symbol t))))
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
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended to `consult-ripgrep-args'."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (project-root (project-current t)))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'identity args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query (or query
                   (when (+fate/region-active-p)
                     (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))))
	 (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                   (concat "\\" (char-to-string separator))
                                   query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                          unless (string-match-p char query)
                                          return char)
                                 "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

;;;###autoload
(defun +fate/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively #'+vertico/project-search-from-cwd)))

;;;###autoload
(defun +fate/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+fate/search-cwd 'other))

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
        (call-interactively #'+vertico/project-search)
      (+vertico/project-search arg))))

;;;###autoload
(defun +fate/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+fate/search-project 'other))
