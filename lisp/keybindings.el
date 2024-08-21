;; -*- lexical-binding: t -*-

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

  (kbd "<leader>bb") #'consult-buffer
  (kbd "<leader>bp") #'consult-project-buffer
  (kbd "<leader>bo") #'consult-buffer-other-window
  (kbd "<leader>bn") #'evil-buffer-new
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
  (kbd "<leader>sb") #'consult-line
  (kbd "<leader>*")  #'+fate/search-project-for-symbol-at-point
  (kbd "<leader>sd") #'+fate/search-cwd
  (kbd "<leader>sD") #'+fate/search-other-cwd
  (kbd "<leader>'")  #'vertico-repeat
  ;; magit
  (kbd "<leader>gg") #'magit-status
  (kbd "<leader>gb") #'magit-blame
  ;; projectile
  (kbd "<leader>SPC")      #'projectile-find-file
  (kbd "<leader>pp")       #'projectile-switch-project
  (kbd "<leader>pi")       #'projectile-invalidate-cache
  (kbd "<leader>pc")       #'projectile-compile-project
  (kbd "<leader>pk")       #'projectile-kill-buffers
  ;; buffers
  (kbd "<leader>br") #'revert-buffer 

  ;; remappings
  (kbd "zx") 'kill-current-buffer
  (kbd "zo") 'evil-toggle-fold
  (kbd "za") 'evil-open-fold)

(evil-define-key 'insert 'global
  (kbd "C-a") #'move-beginning-of-line
  (kbd "C-e") #'move-end-of-line
  (kbd "C-@") #'company-complete-common)

(evil-define-key* '(insert replace visual operator) 'global
  (kbd "\C-g") #'evil-escape)

(define-key minibuffer-mode-map (kbd "C-j") 'next-line)
(define-key minibuffer-mode-map (kbd "C-k") 'previous-line)
(define-key minibuffer-mode-map (kbd "C-w") 'backward-kill-word)
