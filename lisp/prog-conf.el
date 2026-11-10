;; -*- lexical-binding: t -*-

;; Basic treesit setup
(setq treesit-font-lock-level 3)
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     ;; NOTE(hqhs): the grammar symbol is `cpp', not `c++' -- that is what
     ;; `c++-ts-mode' looks for. The old `c++' entry here installed a dylib
     ;; nothing ever loaded, which is why auto-install appeared broken.
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     ;; 404 on github, no yaml repo in tree-sitter project
     ;; (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
     ))

;; Installation helper
(defun +fate/ensure-treesit-languages ()
  "Ensure all tree-sitter language grammars are installed."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (message "Checking grammar for %s" lang)
      (unless (treesit-language-available-p lang)
        (message "Installing grammar for %s" lang)
        (treesit-install-language-grammar lang)))))

;; Language mode remapping
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode) ;; .h files, content-sniffed
        (python-mode     . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode         . js-ts-mode)
        (js2-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        ;; 404 on github, no yaml repo in tree-sitter project
        ;; default highlighting is good enough
        ;; (yaml-mode       . yaml-ts-mode)
        (rust-mode       . rust-ts-mode)
        (go-mode         . go-ts-mode)))

;; Configure indent offset for different modes
(setq c-ts-mode-indent-offset 2
      c++-ts-mode-indent-offset 2
      python-ts-mode-indent-offset 4
      typescript-ts-mode-indent-offset 2
      js-ts-mode-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure 'prog-mode

;; Basic programming defaults
(setq-default indent-tabs-mode nil          ; Use spaces instead of tabs
              tab-width 4                   ; Default tab width
              truncate-lines t              ; Don't wrap lines
              scroll-margin 3               ; Keep 3 lines of context when scrolling
              scroll-conservatively 101     ; Avoid recentering when scrolling far
              scroll-preserve-screen-position t) ; Preserve screen position when scrolling

;; Show matching parentheses
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(add-hook '+fate-first-file-hook #'show-paren-mode)

;; Column indicator and line numbers setup
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
; Line number display configuration
(setq-default display-line-numbers-width 2
              display-line-numbers-width-start nil
              display-line-numbers-grow-only nil
              display-line-numbers-current-absolute nil
              display-line-numbers-type t) ;; t = absolute
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Indentation guide bars
;; NOTE(hqhs): character display rather than stipples. This is a macOS NS
;; build (Emacs 30), which only has partial :stipple support -- garbled or
;; invisible bars. Full NS stipple support lands in Emacs 31; at that point
;; drop `indent-bars-prefer-character' to get the fancy patterned bars.
(use-package indent-bars
  :ensure nil ;; vendored
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-no-stipple-char ?\│)
  (indent-bars-color '(highlight :face-bg t :blend 0.25))
  (indent-bars-highlight-current-depth '(:blend 0.65))
  (indent-bars-display-on-blank-lines t)
  ;; Bar on column 0 too. Default (nil) starts at the first indent position,
  ;; so the outermost body level gets no bar at all.
  (indent-bars-starting-column 0)
  ;; Continuation lines aligned under an open paren (`c-lineup-arglist' style)
  ;; would otherwise spray a bar every `indent-bars-spacing' columns across the
  ;; whole alignment. Cap depth at the line that opened the list instead.
  (indent-bars-no-descend-lists t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module" "translation_unit"))
  :config
  ;; No autoload files are generated for vendored packages, so the treesit
  ;; extension has to be pulled in by hand.
  (when (and indent-bars-treesit-support
             (fboundp 'treesit-available-p)
             (treesit-available-p))
    (require 'indent-bars-ts)))

(add-hook '+fate-first-file-hook #'electric-pair-mode)

;; Highlight TODO/FIXME/NOTE/HACK keywords
(defface +prog-todo-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for TODO keywords.")

(defface +prog-note-face
  '((t (:inherit font-lock-doc-face :weight bold)))
  "Face for NOTE keywords.")

(defun +setup-todo-highlighting ()
  "Add highlighting for TODO keywords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 '+prog-todo-face t)
     ("\\<\\(NOTE\\|HACK\\|XXX\\):" 1 '+prog-note-face t))))

;; Code folding setup with hideshow and outline
(defun +setup-code-folding ()
  "Setup hideshow and outline minor mode."
  (ignore-errors (hs-minor-mode 1))
  (outline-minor-mode 1)
  ;; Define outline regex patterns for common programming constructs
  (setq-local outline-regexp "\\(^\\s-*\\(class\\|public\\|private\\|protected\\|def\\|function\\|if\\|while\\|for\\|do\\)\\)\\|\\(^.*{\\)")
  ;; Make sure evil folding works with hideshow
  (when (boundp 'evil-fold-list)
    (push `((hs-minor-mode)
            :open-all hs-show-all
            :close-all hs-hide-all
            :toggle hs-toggle-hiding
            :open hs-show-block
            :open-rec nil
            :close hs-hide-block)
          evil-fold-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treesit scope breadcrumb in the header line
;;
;; Replaces `which-function-mode', which only reported the enclosing defun in
;; the mode line. This walks the treesit ancestor chain instead, so nested
;; if/for/while show up too:
;;
;;   loco › Half › f16_from_f32 › if (e >= 0x1f) › for (int i = 0; i < 4; ++i)

(defvar +fate-scope-node-types
  '(;; c / c++
    "function_definition" "class_specifier" "struct_specifier"
    "namespace_definition" "enum_specifier"
    "if_statement" "for_statement" "while_statement" "do_statement"
    "switch_statement" "case_statement" "for_range_loop"
    ;; rust
    "function_item" "impl_item" "trait_item" "mod_item"
    "if_expression" "for_expression" "while_expression" "match_expression"
    ;; go
    "function_declaration" "method_declaration" "type_declaration"
    "for_statement" "if_statement" "expression_switch_statement"
    ;; python
    "class_definition" "with_statement" "try_statement")
  "Treesit node types that count as a scope for `+fate/scope-path'.")

(defvar +fate-scope-max-label 40
  "Maximum width of a single component in `+fate/scope-path'.")

(defvar +fate-scope-separator (propertize " › " 'face 'shadow)
  "Separator between components in `+fate/scope-path'.")

(defun +fate--scope-label (node)
  "Return a short label for treesit NODE.
Prefers the defun name; falls back to the node's first source line."
  (or (ignore-errors (treesit-defun-name node))
      (save-excursion
        (goto-char (treesit-node-start node))
        (let ((text (buffer-substring-no-properties (point) (line-end-position))))
          ;; Drop the trailing brace and collapse runs of whitespace.
          (setq text (replace-regexp-in-string "[ \t]*{[ \t]*\\'" "" text))
          (setq text (string-trim (replace-regexp-in-string "[ \t]+" " " text)))
          (truncate-string-to-width text +fate-scope-max-label nil nil t)))))

(defun +fate/scope-path ()
  "Return the treesit scope path at point, or nil outside any scope."
  (when-let* (((fboundp 'treesit-parser-list))
              ((treesit-parser-list))
              (node (treesit-node-at (point))))
    (let (parts)
      (while node
        (when (member (treesit-node-type node) +fate-scope-node-types)
          (let ((label (+fate--scope-label node)))
            ;; A defun's name node can repeat its parent's label.
            (unless (equal label (car parts))
              (push label parts))))
        (setq node (treesit-node-parent node)))
      (when parts
        (string-join parts +fate-scope-separator)))))

(define-minor-mode +fate/scope-header-mode
  "Show the enclosing treesit scope path in the header line."
  :lighter nil
  (setq header-line-format
        (when +fate/scope-header-mode
          ;; Kept unconditionally present so the buffer text does not shift
          ;; up and down as point moves in and out of a scope.
          '(:eval (or (+fate/scope-path) "")))))

(add-hook 'prog-mode-hook #'+fate/scope-header-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight other occurrences of the symbol at point
;;
;; An extension of search rather than a separate feature: the same
;; `lazy-highlight' face isearch uses for its other matches, and `n'/`N' with
;; no pattern adopt the symbol under point (see lisp/evil.el). Plain text
;; matching on symbol boundaries, so comments and strings count.
;;
;; Scoped to the visible window: one screen costs ~0.02ms against ~1.5ms for a
;; 4k-line buffer, and offscreen matches cannot be seen anyway. That is cheap
;; enough to run straight off `post-command-hook' with no idle timer.

(defvar-local +fate--occurrence-overlays nil)

(defvar-local +fate--occurrence-state nil
  "The (SYMBOL WINDOW-START WINDOW-END) the current overlays were built for.")

(defun +fate--occurrence-clear ()
  (mapc #'delete-overlay +fate--occurrence-overlays)
  (setq +fate--occurrence-overlays nil
        +fate--occurrence-state nil))

(defun +fate--occurrence-update ()
  "Highlight occurrences of the symbol at point within the visible window."
  (if (or isearch-mode                            ; isearch highlights its own
          (use-region-p)                          ; do not fight the selection
          (not (eq (current-buffer) (window-buffer))))
      (+fate--occurrence-clear)
    (let* ((symbol (thing-at-point 'symbol t))
           (beg (window-start))
           (end (window-end nil t))
           (state (list symbol beg end)))
      ;; Rebuild only when the symbol or the visible region actually changed;
      ;; moving within one symbol is then free.
      (unless (equal state +fate--occurrence-state)
        (+fate--occurrence-clear)
        (setq +fate--occurrence-state state)
        (when symbol
          (let ((re (concat "\\_<" (regexp-quote symbol) "\\_>")))
            (save-excursion
              (goto-char beg)
              (while (re-search-forward re end t)
                (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ov 'face 'lazy-highlight)
                  (push ov +fate--occurrence-overlays))))))))))

(define-minor-mode +fate/occurrence-mode
  "Highlight other occurrences of the symbol at point."
  :lighter nil
  (if +fate/occurrence-mode
      (add-hook 'post-command-hook #'+fate--occurrence-update nil t)
    (remove-hook 'post-command-hook #'+fate--occurrence-update t)
    (+fate--occurrence-clear)))

(add-hook 'prog-mode-hook #'+fate/occurrence-mode)

(use-package compile
  :ensure nil ;; built-in
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-skip-threshold 2); Skip less important messages
  (compilation-max-output-line-length nil)
  :config
  ;; `compilation-filter-hook' fires once per chunk of process output.
  ;; Colourising (point-min)..(point-max) there is O(n^2) over the course of a
  ;; build; the built-in filter is the same operation scoped to
  ;; `compilation-filter-start'..point.
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Enable visual-line-mode in compilation buffers
  (add-hook 'compilation-mode-hook #'visual-line-mode)

  ;; Nothing bounds a compilation buffer's total size, so a verbose build grows
  ;; until it grinds GC.
  (autoload 'comint-truncate-buffer "comint" nil t)

  (defvar +fate--compilation-truncate-time 0
    "`float-time' of the last compilation-buffer truncation.")

  (defun +fate--compilation-truncate-h ()
    "Truncate the compilation buffer, at most once a second.
`comint-truncate-buffer' counts lines, which is far too expensive to run on
every chunk of process output -- approximate the line count from the buffer
size instead (~80 columns per line) and rate-limit the real check."
    (let ((now (float-time)))
      (when (and (> (buffer-size) (* 80 comint-buffer-maximum-size))
                 (> (- now +fate--compilation-truncate-time) 1))
        (setq +fate--compilation-truncate-time now)
        (+fate/with-delayed-gc!
          (comint-truncate-buffer)))))

  (add-hook 'compilation-filter-hook #'+fate--compilation-truncate-h))

;; Trailing whitespace handling
(setq-default show-trailing-whitespace nil)  ; Disable globally
(add-hook 'prog-mode-hook (lambda ()
                           (setq show-trailing-whitespace t))) ; Enable in prog-mode

;; Main prog-mode hook
(defun +setup-prog-mode ()
  "Setup common programming mode features."
  (electric-indent-mode 1)        ; Electric indentation
  (+setup-todo-highlighting)      ; Highlight TODO keywords
  (+setup-code-folding)          ; Setup code folding
  ;; Enable useful minor modes
  (subword-mode 1))              ; Treat camelCase as separate words

;; Add our setup to prog-mode-hook
(add-hook 'prog-mode-hook #'+setup-prog-mode)

;; Additional useful settings
(setq-default indent-line-function 'indent-relative-first-indent-point)
(setq-default comment-column 40)
(setq-default comment-fill-column 80)

;; Provide better electric indent behavior
(setq-default electric-indent-chars '(?\n ?\} ?\) ?\]))

;; TODO: write custom editorconfig parser (~60 lines)
;; TODO: write custom indent detection heuristic (~40 lines)

;; Whitespace cleanup on save, scoped to lines we actually edited (ws-butler
;; semantics, no dependency). A global `delete-trailing-whitespace' produces a
;; diff full of whitespace hunks on lines nobody looked at, which is
;; unacceptable in a repo we don't own. Markers track edits for free, so
;; recording changed regions costs nothing and `before-save-hook' cleans only
;; those.

(defvar-local +fate--ws-regions nil
  "Marker pairs spanning text changed since the last save.")

(defun +fate--ws-record (beg end &optional _len)
  "Record BEG..END as edited, for `+fate--ws-cleanup'."
  (push (cons (copy-marker beg) (copy-marker end t)) +fate--ws-regions))

(defun +fate--ws-cleanup ()
  "Delete trailing whitespace on lines edited since the last save."
  (save-excursion
    (save-restriction
      (widen)
      (dolist (region +fate--ws-regions)
        (let ((beg (car region))
              (end (cdr region)))
          (when (and (marker-position beg) (marker-position end))
            ;; Expand to whole lines: an edit mid-line still owns its tail.
            (delete-trailing-whitespace
             (save-excursion (goto-char beg) (line-beginning-position))
             (save-excursion (goto-char end) (line-end-position))))
          (set-marker beg nil)
          (set-marker end nil)))))
  (setq +fate--ws-regions nil))

(defun +fate--ws-setup ()
  (add-hook 'after-change-functions #'+fate--ws-record nil t)
  (add-hook 'before-save-hook #'+fate--ws-cleanup nil t))

(add-hook 'prog-mode-hook #'+fate--ws-setup)

;; Async format-on-save (apheleia-style, no external dep)
;; Runs formatter asynchronously after save, then silently re-saves.
;; Uses buffer-hash to detect edits during formatting and abort if needed.

(defvar-local +fate-format-command nil
  "Formatter command as a list of strings. Buffer-local.
The formatter should read stdin and write to stdout.")

(defvar-local +fate--format-process nil
  "Current async formatter process for this buffer.")

(defvar +fate--format-after-save-in-progress nil
  "Non-nil while re-saving after async format, to prevent loops.")

(defun +fate--buffer-hash ()
  "Return a content hash of the current buffer."
  (if (fboundp 'buffer-hash)
      (buffer-hash)
    (md5 (current-buffer))))

(defun +fate--format-apply (buf output-buf saved-hash)
  "Apply formatted OUTPUT-BUF contents to BUF if it hasn't changed since SAVED-HASH."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (if (not (equal saved-hash (+fate--buffer-hash)))
          (message "Fate fmt: buffer changed during format, skipping")
        (let ((orig-point (point)))
          ;; `replace-buffer-contents' allocates heavily for a whole-file diff.
          (+fate/with-delayed-gc!
            (replace-buffer-contents output-buf))
          (goto-char (min orig-point (point-max)))
          ;; Re-save silently with loop guard
          (when buffer-file-name
            (let ((+fate--format-after-save-in-progress t))
              (save-buffer)))))))
  (when (buffer-live-p output-buf)
    (kill-buffer output-buf)))

(defun +fate/format-after-save ()
  "Asynchronously format buffer using `+fate-format-command' after saving."
  (when (and +fate-format-command
             (not +fate--format-after-save-in-progress))
    ;; Kill any in-flight formatter for this buffer
    (when (and +fate--format-process (process-live-p +fate--format-process))
      (delete-process +fate--format-process))
    (let* ((buf (current-buffer))
           (saved-hash (+fate--buffer-hash))
           (output-buf (generate-new-buffer " *fate-fmt*"))
           (cmd (car +fate-format-command))
           (args (cdr +fate-format-command))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (stderr-buf (generate-new-buffer " *fate-fmt-stderr*"))
           (proc (make-process
                  :name "fate-fmt"
                  :buffer output-buf
                  :command (cons cmd args)
                  :connection-type 'pipe
                  :noquery t
                  :stderr stderr-buf
                  :sentinel
                  (lambda (proc _event)
                    (unless (process-live-p proc)
                      (if (zerop (process-exit-status proc))
                          (+fate--format-apply buf output-buf saved-hash)
                        (let ((err (and (buffer-live-p stderr-buf)
                                        (with-current-buffer stderr-buf
                                          (string-trim (buffer-string))))))
                          (message "Fate fmt: %s failed (exit %d)%s"
                                   cmd (process-exit-status proc)
                                   (if (and err (not (string-empty-p err)))
                                       (concat "\n" err) "")))
                        (when (buffer-live-p output-buf)
                          (kill-buffer output-buf)))
                      (when (buffer-live-p stderr-buf)
                        (kill-buffer stderr-buf)))))))
      (setq +fate--format-process proc)
      (process-send-string proc content)
      (process-send-eof proc))))

(add-hook 'after-save-hook #'+fate/format-after-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep `tab-width' in step with the mode's own indent offset
;;
;; The offsets above are set per mode, but `tab-width' stays 4 globally -- so a
;; literal tab in a C file displays at 4 columns while the mode indents at 2.
;; Doom derives the mapping with a name-based heuristic plus a registry of
;; exceptions; we know our modes, so a plain alist does the whole job.

(defvar +fate-indent-var-alist
  '((c-mode              . c-basic-offset)
    (c++-mode            . c-basic-offset)
    (c-ts-mode           . c-ts-mode-indent-offset)
    (c++-ts-mode         . c-ts-mode-indent-offset)
    (python-ts-mode      . python-ts-mode-indent-offset)
    (python-mode         . python-indent-offset)
    (js-ts-mode          . js-ts-mode-indent-offset)
    (typescript-ts-mode  . typescript-ts-mode-indent-offset)
    (rust-ts-mode        . rust-ts-mode-indent-offset)
    (go-ts-mode          . go-ts-mode-indent-offset)
    (yaml-mode           . yaml-indent-offset)
    (cmake-mode          . cmake-tab-width)
    (emacs-lisp-mode     . lisp-indent-offset))
  "Major mode -> the variable that mode indents by.")

(defun +fate-set-indent ()
  "Sync `tab-width' and `standard-indent' to the major mode's indent offset."
  (when-let* ((var (cdr (assq major-mode +fate-indent-var-alist)))
              ((boundp var))
              (width (symbol-value var))
              ((integerp width)))
    (setq-local tab-width width
                standard-indent width)))

;; `change-major-mode-after-body-hook' runs after the mode body has set its own
;; variables but before mode hooks, so a hook can still override this.
(add-hook 'change-major-mode-after-body-hook #'+fate-set-indent)

;; TODO: multiple cursors support

(provide 'prog-conf)
