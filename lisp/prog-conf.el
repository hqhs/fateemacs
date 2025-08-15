;; -*- lexical-binding: t -*-

;; Basic treesit setup
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     ;; NOTE(hqhs): treesit for c/c++ is optional because auto install doesn't
     ;; work
     ;; (c "https://github.com/tree-sitter/tree-sitter-c")
     ;; (c++ "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4" nil "c++")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     ;; 404 on github, no yaml repo in tree-sitter project
     ;; (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
     (json "https://github.com/tree-sitter/tree-sitter-json")))

(defun +fate/treesit-manual-install ()
  "Manually install C++ grammar for tree-sitter on macOS."
  (interactive)
  ;; FIXME(hqhs): breaks if `--init-directory' is not ~/.emacs.d
  (let* ((default-directory "~/.emacs.d/tree-sitter/")
         (cpp-dir (expand-file-name "cpp-grammar"))
         (process-environment
          (append process-environment
                  (list "CC=cc"
                        "CXX=c++"
                        ;; These flags are important for macOS compilation
                        "CFLAGS=-fPIC -std=c11"
                        "CXXFLAGS=-fPIC -std=c++11"))))

    ;; Create directory
    (make-directory cpp-dir t)

    ;; Clone repo if needed
    (unless (file-exists-p (expand-file-name ".git" cpp-dir))
      (call-process "git" nil t t "clone"
                   "https://github.com/tree-sitter/tree-sitter-cpp.git"
                   cpp-dir))

    (let ((default-directory cpp-dir))
      ;; On macOS, we need to:
      ;; 1. Compile scanner.c with C11 standard
      ;; 2. Compile parser.c
      ;; 3. Link everything into a dylib
      (shell-command
       (concat
        "cc -fPIC -std=c11 -c -I./src/ ./src/scanner.c && "
        "cc -fPIC -std=c11 -c -I./src/ ./src/parser.c && "
        "c++ -fPIC -std=c++11 -dynamiclib *.o -o ../libtree-sitter-cpp.dylib")))))

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
      '(;; NOTE(hqhs): enabled optionally based on availability in cc.el
        ;; (c-mode          . c-ts-mode)
        ;; (c++-mode        . c++-ts-mode)
        (python-mode     . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode         . js-ts-mode)
        (js2-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        ;; 404 on github, no yaml repo in tree-sitter project
        ;; default highlighting is good enough
        ;; (yaml-mode       . yaml-ts-mode)
        (rust-mode       . rust-ts-mode)
        (go-mode         . go-ts-mode)))

;; Tree-sitter face customization
(defun +setup-treesit-faces ()
  "Setup enhanced tree-sitter highlighting faces."
  (custom-set-faces
   ;; Function and method declarations
   '(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face :weight normal))))
   '(tree-sitter-hl-face:function.method.call ((t (:inherit tree-sitter-hl-face:function.call))))
   '(tree-sitter-hl-face:constructor ((t (:inherit tree-sitter-hl-face:function.call :weight bold))))

   ;; Variables and properties
   '(tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face :slant normal))))
   '(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
   '(tree-sitter-hl-face:variable.parameter ((t (:inherit tree-sitter-hl-face:variable :slant italic))))

   ;; Types and attributes
   '(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face :weight bold))))
   '(tree-sitter-hl-face:type.argument ((t (:inherit tree-sitter-hl-face:type :weight normal))))
   '(tree-sitter-hl-face:type.parameter ((t (:inherit tree-sitter-hl-face:type :weight normal :slant italic))))
   '(tree-sitter-hl-face:attribute ((t (:inherit font-lock-preprocessor-face :slant italic))))

   ;; Keywords and operators
   '(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face :weight bold))))
   '(tree-sitter-hl-face:operator ((t (:inherit font-lock-operator-face :weight bold))))

   ;; Constants and literals
   '(tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
   '(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-builtin-face :weight bold))))
   '(tree-sitter-hl-face:string ((t (:inherit font-lock-string-face))))
   '(tree-sitter-hl-face:string.special ((t (:inherit tree-sitter-hl-face:string :weight bold))))
   '(tree-sitter-hl-face:embedded ((t (:inherit font-lock-variable-name-face :background "#232531"))))

   ;; Comments and documentation
   '(tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
   '(tree-sitter-hl-face:doc ((t (:inherit font-lock-doc-face))))

   ;; Punctuation and brackets
   '(tree-sitter-hl-face:punctuation.bracket ((t (:inherit font-lock-bracket-face :weight normal))))
   '(tree-sitter-hl-face:punctuation.delimiter ((t (:inherit font-lock-delimiter-face :weight normal))))

   ;; Labels and tags
   '(tree-sitter-hl-face:label ((t (:inherit font-lock-property-face :weight bold))))
   '(tree-sitter-hl-face:tag ((t (:inherit font-lock-function-name-face :weight bold)))))

  ;; Set up general tree-sitter configuration
  (setq treesit-font-lock-level 3)  ; Maximum highlighting level
  )

;; Configure indent offset for different modes
(setq c-ts-mode-indent-offset 2
      c++-ts-mode-indent-offset 2
      python-ts-mode-indent-offset 4
      typescript-ts-mode-indent-offset 2
      js-ts-mode-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure 'prog-mode

;; Add tree-sitter setup to prog-mode-hook
(add-hook 'prog-mode-hook #'+setup-treesit-faces)

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
(show-paren-mode 1)

;; Column indicator and line numbers setup
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
; Line number display configuration
(setq-default display-line-numbers-width 2
              display-line-numbers-width-start nil
              display-line-numbers-grow-only nil
              display-line-numbers-current-absolute nil
              display-line-numbers-type 'relative)
;; I use evilem motions instead
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Electric pair mode (alternative to smartparens for basic needs)
(use-package electric
  :ensure nil ;; built-in
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-open-newline-between-pairs t) ;; default, but still
  :config
  (electric-pair-mode 1))

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

;; Which function mode setup
(defun +setup-which-function ()
  "Setup which-function-mode with better defaults."
  (which-function-mode 1)
  (setq which-func-unknown "⊥") ; Show this when function is unknown
  ;; Only show which-function in mode-line when it's available
  (setq mode-line-misc-info
        (delete (assoc 'which-function-mode
                      mode-line-misc-info) mode-line-misc-info))
  (setq mode-line-misc-info
        (append mode-line-misc-info
                '((which-function-mode ("" which-func-format))))))

(use-package compile
  :ensure nil ;; built-in
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-skip-threshold 2); Skip less important messages
  (compilation-max-output-line-length nil)
  :config
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
  ;; Enable visual-line-mode in compilation buffers
  (add-hook 'compilation-mode-hook #'visual-line-mode)
  )

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
  (+setup-which-function)        ; Show current function in mode-line
  ;; Enable useful minor modes
  (subword-mode 1)               ; Treat camelCase as separate words
  (show-paren-mode 1))           ; Show matching parentheses

;; Add our setup to prog-mode-hook
(add-hook 'prog-mode-hook #'+setup-prog-mode)

;; Additional useful settings
(setq-default indent-line-function 'indent-relative-first-indent-point)
(setq-default comment-column 40)
(setq-default comment-fill-column 80)

;; Provide better electric indent behavior
(setq-default electric-indent-chars '(?\n ?\} ?\) ?\]))

(use-package editorconfig
  :straight t
  :init
  (editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(use-package dtrt-indent
  :straight t
  :commands dtrt-indent-mode
  :hook (prog-mode . dtrt-indent-mode)
  :config
  ;; TODO: what is smie mode?
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t
        ;; reduced from the default of 5000
        dtrt-indent-max-lines 2000)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

(use-package ws-butler
  :straight t
  :init
    ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil
        ws-butler-global-exempt-modes '(python-mode python-ts-mode))
  :config
  (ws-butler-global-mode))

(defun +fate/sp-escape-and-remove-overlay ()
  "Combine sp-remove-active-pair-overlay and +fate/sp-escape-and-evil-escape."
  (interactive)
  (sp-remove-active-pair-overlay)
  (when (and (bound-and-true-p evil-mode)
             (bound-and-true-p evil-escape-mode)
             (eq evil-state 'insert))
    (evil-escape)))

(use-package format-all
  :hook (rust-ts-mode . format-all-mode))


;; TODO: multiple cursors support

(provide 'prog-conf)
