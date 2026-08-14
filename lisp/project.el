;; -*- lexical-binding: t -*-

;; Built into Emacs, no need for use-package or straight
(require 'project)

;; Configure project.el behavior
(setq project-switch-commands
      '((project-find-file "Find file" "f")
        (project-find-regexp "Find regexp" "g")
        (project-dired "Find directory" "d")
        (project-eshell "Eshell" "e")
        (+fate/project-compile "Compile" "c")))

;; By default, project.el searches for either .git, .hg, etc.
;; Add more root markers here if needed
(setq project-find-functions
      '(project-try-vc
        ;; Add any custom project root finding functions
        ))

;; Store recent projects in fate-cache-dir
(setq project-list-file (concat fate-cache-dir "projects"))

;; Bind keys in the project map (accessed via C-x p by default)
(defvar +fate--project-compile-commands (make-hash-table :test 'equal)
  "Hash table mapping project roots to their compile commands.")

(defun +fate/project-compile ()
  "Like `project-compile', but remembers the compile command per project."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (saved (gethash root +fate--project-compile-commands))
         (compile-command (or saved compile-command)))
    (call-interactively #'compile)
    (puthash root compile-command +fate--project-compile-commands)))

(add-to-list 'savehist-additional-variables '+fate--project-compile-commands)


;;; Session marks (per-project, in-memory only) --------------------------
;; Quick jump-points scoped to the current project. Two deliberate
;; differences from `bookmark.el':
;;   1. Project-scoped: the searchable list only shows marks for the
;;      project you're in, keyed by project root.
;;   2. Session-only: the hash table below is NOT added to
;;      `savehist-additional-variables', so nothing ever touches disk.
;; Each mark follows edits the way a real Emacs mark does, via two
;; mechanisms:
;;   - a live `point-marker', which rides along with insertions and
;;     deletions for as long as the buffer stays open (any file size);
;;   - a `bookmark-make-record' result, used only after the buffer was
;;     killed and reopened, to relocate via bookmark's front/rear
;;     context-string search. (Caveat: bookmark only stores that context
;;     when >=16 chars surround point -- see `bookmark-search-size' -- so
;;     near the very top/bottom of a tiny file the reopen path falls back
;;     to the saved position. The live-marker path has no such limit.)
(require 'bookmark)

(defvar +fate--project-marks (make-hash-table :test 'equal)
  "Map of project root -> list of session marks (newest first).
Each entry is a plist (:marker :record :label :file :line).
Session-local: never persisted.")

(defun +fate--project-marks-root ()
  "Project root for the current buffer, or `default-directory'."
  (if-let ((proj (project-current nil)))
      (project-root proj)
    default-directory))

(defun +fate/mark-set ()
  "Set a session mark at point, scoped to the current project.
The mark relocates with edits via bookmark context strings.
Re-marking the same file+line replaces the existing mark."
  (interactive)
  (let* ((root (+fate--project-marks-root))
         (file (buffer-file-name))
         (line (line-number-at-pos))
         (text (string-trim
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
         (label (format "%s:%d  %s"
                        (if file (file-relative-name file root) (buffer-name))
                        line
                        (if (string-empty-p text) "(blank line)" text)))
         (entry (list :marker (point-marker) :record (bookmark-make-record)
                      :label label :file file :line line))
         ;; drop any prior mark on the same file+line
         (marks (cl-remove-if (lambda (m)
                                (and (equal (plist-get m :file) file)
                                     (eql (plist-get m :line) line)))
                              (gethash root +fate--project-marks))))
    (puthash root (cons entry marks) +fate--project-marks)
    (message "Marked: %s" label)))

(defun +fate/mark-jump ()
  "Jump to a session mark in the current project (searchable)."
  (interactive)
  (let* ((root (+fate--project-marks-root))
         (marks (gethash root +fate--project-marks)))
    (unless marks (user-error "No session marks in this project"))
    (let* ((table (mapcar (lambda (m) (cons (plist-get m :label) m)) marks))
           (choice (completing-read "Jump to mark: " table nil t))
           (entry (cdr (assoc choice table)))
           (marker (plist-get entry :marker)))
      (when entry
        (if (and marker (marker-buffer marker))
            ;; Buffer still open: the marker has tracked every edit.
            (progn (switch-to-buffer (marker-buffer marker))
                   (goto-char marker))
          ;; Buffer was killed: reopen and relocate via bookmark context.
          (bookmark-jump (plist-get entry :record)))
        (recenter)))))

(defun +fate/mark-clear ()
  "Forget all session marks for the current project."
  (interactive)
  (remhash (+fate--project-marks-root) +fate--project-marks)
  (message "Cleared session marks for this project"))

(define-key project-prefix-map (kbd "m") #'+fate/project-compile)
(define-key project-prefix-map (kbd "k") #'project-kill-buffers)

;; Optional: Configure project switching behavior
(setq project-switch-use-entire-frame t)  ; Use full frame for project commands
(setq project-kill-buffers-display-buffer-list t)  ; Show buffer list when killing

;; Optional: Make project commands available in more buffers
(setq project-switch-commands-respect-buffer t)

;; Optional: Configure project VC integration
(setq project-vc-merge-submodules nil)  ; Don't descend into git submodules
(setq xref-search-program 'ripgrep)
(setq project-vc-ignores '(".git" ".hg" ".bzr" "_darcs"))

;; Optional: Dired integration
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c p") project-prefix-map))

(provide 'project-config)
