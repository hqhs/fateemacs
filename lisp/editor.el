;; -*- lexical-binding: t -*-

;; Basic treesit setup
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     ;; (c++ "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4" nil "c++")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     ;; (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
     (json "https://github.com/tree-sitter/tree-sitter-json")))

(defun +fate/treesit-manual-install ()
  "Manually install C++ grammar for tree-sitter on macOS."
  (interactive)
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
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (python-mode     . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode         . js-ts-mode)
        (js2-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (rust-mode       . rust-ts-mode)
        (go-mode         . go-ts-mode)))

;; Configure indent offset for different modes
(setq c-ts-mode-indent-offset 2
      c++-ts-mode-indent-offset 2
      python-ts-mode-indent-offset 4
      typescript-ts-mode-indent-offset 2
      js-ts-mode-indent-offset 2)

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
  :commands dtrt-indent-mode
  :straight t
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

  (setq ws-butler-keep-whitespace-before-point nil)
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

(use-package smartparens
  :straight t
  ;; :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (smartparens-global-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)
  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  (defvar fate-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
	     (defun fate-enable-smartparens-mode-maybe-h ()
	       (when fate-buffer-smartparens-mode
		 (turn-on-smartparens-mode)
		 (kill-local-variable 'fate-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
	     (defun fate-disable-smartparens-mode-maybe-h ()
	       (when smartparens-mode
		 (setq-local fate-buffer-smartparens-mode t)
		 (turn-off-smartparens-mode))))
  )


(use-package pcre2el
  :straight t
  :commands (rxt-quote-pcre))

;; TODO: multiple cursors support
