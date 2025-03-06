;; -*- lexical-binding: t -*-

(global-unset-key (kbd "C-<wheel-up>"))   ;; Text scale increase
(global-unset-key (kbd "C-<wheel-down>")) ;; Text scale decrease
(global-unset-key (kbd "C-<wheel-left>"))
(global-unset-key (kbd "C-<wheel-right>"))

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
  (kbd "<leader>,") #'ido-switch-buffer ;;consult-buffer
  (kbd "<leader><") #'project-switch-to-buffer ;; consult-project-buffer
  (kbd "<leader>.") #'find-file

  ;; "buffers" in emacs lingo, but configured as "file" prefix since
  ;; "f" is easier to type
  (kbd "<leader>fi") #'ibuffer
  (kbd "<leader>ff") #'ido-switch-buffer
  (kbd "<leader>fp") #'project-switch-to-buffer
  (kbd "<leader>fo") #'consult-buffer-other-window
  (kbd "<leader>fn") #'evil-buffer-new
  (kbd "<leader>fr") #'revert-buffer
  (kbd "<leader>fs") #'save-buffer
  (kbd "<leader>fS") #'write-file           ; Save as
  (kbd "<leader>fD") #'delete-file
  (kbd "<leader>fR") #'rename-file
  ;; TODO(hqhs): two more: copy debugger-formatted location
  ;; and copy the whole buffer
  (kbd "<leader>fy") #'+fate/copy-file-path ; Add this helper function

  ;; bookmarks (another reason to prefix "buffers" as files)
  ;; also prefixed with C-x r

  (kbd "<leader>bb") #'bookmark-jump
  (kbd "<leader>bl") #'list-bookmarks
  (kbd "<leader>bm") #'bookmark-set

  ;; outline!
  (kbd "<leader>oa") #'outline-show-all
  (kbd "<leader>oh") #'outline-hide-sublevels

  ;; xref
  (kbd "<leader>xa") #'xref-find-apropos
  (kbd "<leader>xd") #'xref-find-definitions
  (kbd "<leader>xr") #'xref-find-references

  ;; compilation
  (kbd "<leader>cc") #'compile
  (kbd "<leader>cr") #'recompile
  (kbd "<leader>ck") #'kill-compilation
  ;; comments
  (kbd "<leader>cd") #'comment-dwim
  (kbd "<leader>cl") #'comment-line
  ;; case conversion
  (kbd "<leader>cu") #'upcase-dwim
  (kbd "<leader>cl") #'downcase-dwim

  ;; search
  (kbd "<leader>si") #'consult-imenu
  (kbd "<leader>sp") #'consult-ripgrep
  (kbd "<leader>sP") #'+fate/search-other-project
  (kbd "<leader>sf") #'consult-line
  (kbd "<leader>*")  #'+fate/search-project-for-symbol-at-point
  (kbd "<leader>sd") #'+fate/search-cwd
  (kbd "<leader>sD") #'+fate/search-other-cwd
  ;; (kbd "<leader>'")  #'vertico-repeat

  ;; toggle functions
  (kbd "<leader>tl") #'display-line-numbers-mode
  (kbd "<leader>tw") #'whitespace-mode
  (kbd "<leader>tf") #'auto-fill-mode
  (kbd "<leader>th") #'hl-line-mode
  (kbd "<leader>tv") #'visual-line-mode
  (kbd "<leader>tV") #'global-visual-line-mode

  ;; magit
  (kbd "<leader>gg") #'magit-status
  (kbd "<leader>gb") #'magit-blame

  ;; project
  (kbd "<leader>SPC") #'project-find-file
  (kbd "<leader>pp")  #'project-switch-project
  (kbd "<leader>pc")  #'project-compile
  (kbd "<leader>pk")  #'project-kill-buffers
  (kbd "<leader>pd")  #'project-dired
  (kbd "<leader>ps")  #'project-shell
  (kbd "<leader>pg")  #'project-find-regexp ;; NOTE(hqhs): produces new buffer with results
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
  (kbd "C-SPC") #'company-complete-common)

;; (evil-define-key* '(insert replace visual operator) 'global
;;   (kbd "<escape>") #'evil-escape)

(define-key minibuffer-mode-map (kbd "C-j") 'next-line)
(define-key minibuffer-mode-map (kbd "C-k") 'previous-line)
(define-key minibuffer-mode-map (kbd "C-w") 'backward-kill-word)
