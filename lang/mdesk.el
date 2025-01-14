;; -*- lexical-binding: t -*-

(defface mdesk-code-face
  '((t :inherit font-lock-preprocessor-face :slant italic))
  "Face for code sections in backticks that will be generated.")

(defface mdesk-table-name-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for table names in MDesk files.")

(defvar mdesk-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Make - part of comments for //-style
    (modify-syntax-entry ?- "." table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Make backtick just a regular character (not string delimiter)
    (modify-syntax-entry ?\` "." table)
    ;; Symbol constituents
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table for MDesk mode.")

(defvar mdesk-font-lock-keywords
  `(;; Table declarations
    ("^\\(@table\\)\\s-*(\\([^)]+\\))"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ;; Table names with colon
    ("^\\([A-Za-z0-9_]+\\):" 1 'mdesk-table-name-face)
    ;; MDesk directives
    ("^\\s-*\\(@\\(?:gen\\|expand\\|c_file\\)\\)\\>" 1 font-lock-keyword-face)
    ;; Backtick-delimited code, handling both single and multiline
    ("`\\([^`\n]+\\|[^`]+\\)`" 1 'mdesk-code-face t)
    ;; Variable interpolation ${...}
    ("\\${\\([^}\n]+\\)}" 1 font-lock-variable-name-face)
    ;; Comments starting with //-
    ("//[-]+.*$" 0 font-lock-comment-face t)
    ;; Table field definitions
    ("^\\s-*{\\s-*\\([A-Za-z0-9_]+\\)\\s-+\\([A-Za-z0-9_]+\\)\\s-+\\([^}\n]+\\)}"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-type-face))
    ;; Basic C/type keywords
    ("\\<\\(if\\|else\\|while\\|for\\|return\\|struct\\|typedef\\)\\>" . font-lock-keyword-face)
    ("\\<\\(void\\|int\\|char\\|float\\|double\\|bool32\\|f32\\|u32\\)\\>" . font-lock-type-face))
  "Highlighting for MDesk mode.")

(define-derived-mode mdesk-mode prog-mode "MDesk"
  "Major mode for editing MDesk template files."
  :syntax-table mdesk-mode-syntax-table
  (setq-local font-lock-defaults '(mdesk-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  ;; Improve indentation a bit
  (setq-local indent-line-function 'indent-relative))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mdesk\\'" . mdesk-mode))

(provide 'mdesk-mode)
