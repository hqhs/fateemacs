;; -*- lexical-binding: t -*-

;; C-c C-c comment region

(evil-ex-define-cmd "W[rite]" #'evil-write) ;; fix for common error

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key 'normal 'global
  (kbd "K") #'eldoc)

(evil-define-key '(normal visual operator) 'global
  (kbd "#") #'+fate/search-symbol-backward
  (kbd "*") #'+fate/search-symbol-forward
  ;; UI
  (kbd "<leader>ub") #'+fate/toggle-background-transparency
  ;;
  ;; to narrow buffers, use 'consult-narrow-key (currently defined as "<")
  (kbd "<leader>,") #'consult-buffer
  (kbd "<leader><") #'consult-project-buffer
  (kbd "<leader>.") #'find-file

  ;; "buffers" in emacs lingo, but configured as "file" prefix since
  ;; "f" is easier to type
  (kbd "<leader>ff") #'consult-buffer ;; also "find file"
  (kbd "<leader>fp") #'consult-project-buffer ;; // same as <leader>pf
  (kbd "<leader>fo") #'consult-buffer-other-window
  (kbd "<leader>fn") #'evil-buffer-new
  (kbd "<leader>fr") #'revert-buffer

  ;; bookmarks (another reason to prefix "buffers" as files)
  ;; also prefixed with C-x r

  (kbd "<leader>bb") #'bookmark-jump
  (kbd "<leader>bl") #'list-bookmarks
  (kbd "<leader>bm") #'bookmark-set

  ;; outline!
  (kbd "<leader>oa") #'outline-show-all
  (kbd "<leader>oh") #'outline-hide-sublevels

  ;; code
  (kbd "<leader>ca") #'xref-find-apropos
  (kbd "<leader>cd") #'xref-find-definitions
  (kbd "<leader>cr") #'xref-find-references

  ;; search
  (kbd "<leader>si") #'consult-imenu
  (kbd "<leader>sp") #'consult-ripgrep
  (kbd "<leader>sP") #'+fate/search-other-project
  (kbd "<leader>sf") #'consult-line
  (kbd "<leader>*")  #'+fate/search-project-for-symbol-at-point
  (kbd "<leader>sd") #'+fate/search-cwd
  (kbd "<leader>sD") #'+fate/search-other-cwd
  (kbd "<leader>'")  #'vertico-repeat

  ;; magit
  (kbd "<leader>gg") #'magit-status
  (kbd "<leader>gb") #'magit-blame

  ;; project
  (kbd "<leader>SPC") #'project-find-file
  (kbd "<leader>pp")  #'project-switch-project
  (kbd "<leader>pc")  #'project-compile
  (kbd "<leader>pk")  #'project-kill-buffers
  ;; Additional useful bindings
  (kbd "<leader>pd")  #'project-dired
  (kbd "<leader>ps")  #'project-shell
  (kbd "<leader>pg")  #'project-find-regexp
  (kbd "<leader>pr")  #'project-query-replace-regexp
  (kbd "<leader>p!")  #'project-shell-command
  (kbd "<leader>p&")  #'project-async-shell-command

  ;; remappings
  (kbd "zx") 'kill-current-buffer
  (kbd "zo") 'evil-toggle-fold
  (kbd "za") 'evil-open-fold)

(evil-define-key 'insert 'global
  (kbd "C-a") #'move-beginning-of-line
  (kbd "C-e") #'move-end-of-line
  (kbd "C-@") #'company-complete-common)

(evil-define-key* '(insert replace visual operator) 'global
  (kbd "<escape>") #'evil-escape)

(define-key minibuffer-mode-map (kbd "C-j") 'next-line)
(define-key minibuffer-mode-map (kbd "C-k") 'previous-line)
(define-key minibuffer-mode-map (kbd "C-w") 'backward-kill-word)
