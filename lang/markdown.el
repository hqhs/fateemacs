;; -*- lexical-binding: t -*-

;;; Minimal markdown highlighting mode, extracted from markdown-mode.
;;; Font-lock only — no export, preview, or table editing.

;; Faces

(defface +md-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for markdown headings.")

(defface +md-header-face-1 '((t (:inherit +md-header-face :height 1.3))) "H1.")
(defface +md-header-face-2 '((t (:inherit +md-header-face :height 1.2))) "H2.")
(defface +md-header-face-3 '((t (:inherit +md-header-face :height 1.1))) "H3.")
(defface +md-header-face-4 '((t (:inherit +md-header-face)))             "H4.")
(defface +md-header-face-5 '((t (:inherit +md-header-face)))             "H5.")
(defface +md-header-face-6 '((t (:inherit +md-header-face)))             "H6.")

(defface +md-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Delimiters and markup characters.")

(defface +md-bold-face    '((t (:inherit bold)))   "Bold text.")
(defface +md-italic-face  '((t (:inherit italic))) "Italic text.")
(defface +md-strike-face  '((t (:strike-through t))) "Strikethrough text.")

(defface +md-code-face
  '((t (:inherit font-lock-constant-face)))
  "Inline code.")

(defface +md-pre-face
  '((t (:inherit font-lock-builtin-face)))
  "Fenced/indented code blocks.")

(defface +md-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Blockquote text.")

(defface +md-list-face
  '((t (:inherit +md-markup-face)))
  "List markers.")

(defface +md-link-face
  '((t (:inherit link)))
  "Link text.")

(defface +md-url-face
  '((t (:inherit font-lock-string-face)))
  "URLs.")

(defface +md-hr-face
  '((t (:inherit +md-markup-face)))
  "Horizontal rules.")

;; Regexes

(defconst +md-re-heading
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "ATX heading: group 1 = hashes, group 2 = text.")

(defconst +md-re-setext
  "^\\([^\r\n\t -].*\\)\n\\(=+\\|-+\\)$"
  "Setext heading: group 1 = text, group 2 = underline.")

(defconst +md-re-bold
  "\\(?:^\\|[^\\]\\)\\(\\(\\*\\*\\|__\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\2\\)\\)"
  "Bold: group 2,4 = delimiters, group 3 = content.")

(defconst +md-re-italic
  "\\(?:^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\2\\)\\)"
  "Italic: group 2,4 = delimiters, group 3 = content.")

(defconst +md-re-strike
  "\\(?:^\\|[^\\]\\)\\(\\(~~\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(~~\\)\\)"
  "Strikethrough: group 2,4 = delimiters, group 3 = content.")

(defconst +md-re-code
  "\\(?:\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(?:.\\|\n[^\n]\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)"
  "Inline code: group 2 = open ticks, group 3 = code, group 4 = close.")

(defconst +md-re-blockquote
  "^[ \t]*\\(>\\)\\([ \t]*\\)\\(.*\\)$"
  "Blockquote: group 1 = marker, group 3 = text.")

(defconst +md-re-list
  "^[ \t]*\\([*+:-]\\|[0-9]+[.)]\\)[ \t]+"
  "List item marker.")

(defconst +md-re-hr
  "^[ \t]*\\(\\*[ \t]*\\*[ \t]*\\*[* \t]*\\|-[ \t]*-[ \t]*-[- \t]*\\|_[ \t]*_[ \t]*_[_ \t]*\\)$"
  "Horizontal rule.")

(defconst +md-re-fence-open
  "^[[:blank:]]*\\(`\\{3,\\}\\)[[:blank:]]*\\([^`[:space:]]+\\)?.*$"
  "Fenced code block open: group 1 = fence, group 2 = language.")

(defconst +md-re-fence-close
  "^[[:blank:]]*\\(`\\{3,\\}\\)[[:blank:]]*$"
  "Fenced code block close.")

(defconst +md-re-pre
  "^\\(    \\|\t\\).*$"
  "Indented code block.")

(defconst +md-re-link
  "\\(!?\\)\\(\\[\\)\\([^]]*\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\()\\)"
  "Inline link: group 1 = !, group 3 = text, group 6 = url.")

(defconst +md-re-refdef
  "^ \\{0,3\\}\\(\\[\\)\\([^]\n]+?\\)\\(\\]\\)\\(:\\)\\s-*\\(.*?\\)\\s-*$"
  "Reference definition: group 2 = label, group 5 = url.")

;; Heading fontifier — picks face by level

(defun +md--fontify-heading (limit)
  "Search for ATX headings up to LIMIT and apply level-appropriate face."
  (when (re-search-forward +md-re-heading limit t)
    (let* ((level (min 6 (length (match-string 1))))
           (face (intern (format "+md-header-face-%d" level))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'face '+md-markup-face)
      (put-text-property (match-beginning 2) (match-end 2)
                         'face face)
      (when (< (match-beginning 3) (match-end 3))
        (put-text-property (match-beginning 3) (match-end 3)
                           'face '+md-markup-face)))
    t))

;; Font-lock keywords

(defvar +md-font-lock-keywords
  `((+md--fontify-heading)
    (,+md-re-setext (1 '+md-header-face-1 t)
                    (2 '+md-markup-face t))
    (,+md-re-bold (2 '+md-markup-face t)
                  (3 '+md-bold-face t)
                  (4 '+md-markup-face t))
    (,+md-re-italic (2 '+md-markup-face t)
                    (3 '+md-italic-face t)
                    (4 '+md-markup-face t))
    (,+md-re-strike (2 '+md-markup-face t)
                    (3 '+md-strike-face t)
                    (4 '+md-markup-face t))
    (,+md-re-code (2 '+md-markup-face t)
                  (3 '+md-code-face t)
                  (4 '+md-markup-face t))
    (,+md-re-blockquote (1 '+md-markup-face)
                        (3 '+md-blockquote-face))
    (,+md-re-list (1 '+md-list-face))
    (,+md-re-hr (1 '+md-hr-face))
    (,+md-re-fence-open (1 '+md-markup-face)
                        (2 'font-lock-type-face nil t))
    (,+md-re-fence-close (1 '+md-markup-face))
    (,+md-re-pre (0 '+md-pre-face))
    (,+md-re-link (2 '+md-markup-face)
                  (3 '+md-link-face)
                  (4 '+md-markup-face)
                  (5 '+md-markup-face)
                  (6 '+md-url-face)
                  (7 '+md-markup-face))
    (,+md-re-refdef (1 '+md-markup-face)
                    (2 '+md-link-face)
                    (3 '+md-markup-face)
                    (4 '+md-markup-face)
                    (5 '+md-url-face)))
  "Font-lock keywords for `+fate-markdown-mode'.")

;; Syntax table

(defvar +md-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (dolist (c '(?\" ?$ ?% ?* ?+ ?/ ?< ?= ?> ?_ ?| ?& ?'))
      (modify-syntax-entry c "." st))
    st)
  "Syntax table for `+fate-markdown-mode'.")

;; Mode definition

(define-derived-mode +fate-markdown-mode text-mode "Markdown"
  "Minimal markdown mode with font-lock highlighting."
  :syntax-table +md-syntax-table
  (setq font-lock-defaults
        '(+md-font-lock-keywords nil nil nil nil
          (font-lock-multiline . t)))
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->"))

(add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mdx\\)\\'" . +fate-markdown-mode))

(provide 'markdown)
