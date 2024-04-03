;; -*- lexical-binding: t -*-

(evil-ex-define-cmd "W[rite]" #'evil-write) ;; fix for common error

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key 'normal 'global
  (kbd "K") #'eldoc)

(evil-define-key '(normal visual operator) 'global
  (kbd "#") #'+fate/search-symbol-backward
  (kbd "*") #'+fate/search-symbol-forward
  ;;
  (kbd "<leader>,") #'consult-buffer
  (kbd "<leader>.") #'find-file
  ;; search
  (kbd "<leader>si") #'consult-imenu
  (kbd "<leader>sp") #'consult-ripgrep
  (kbd "<leader>sb") #'consult-line
  (kbd "<leader>*")  #'+fate/search-project-for-symbol-at-point
  (kbd "<leader>'")  #'vertico-repeat
  ;; magit
  (kbd "<leader>gg") #'magit-status
  (kbd "<leader>gb") #'magit-blame
  ;; projectile
  (kbd "<leader>SPC")      #'projectile-find-file
  (kbd "<leader>pp")       #'projectile-switch-project
  (kbd "<leader>pi")       #'projectile-invalidate-cache
  ;; buffers
  (kbd "<leader>br") #'revert-buffer 

  ;; remappings
  (kbd "zx") 'kill-current-buffer)

(evil-define-key 'insert 'global
  (kbd "C-@") #'company-complete-common)

(evil-define-key* '(insert replace visual operator) 'global
  (kbd "\C-g") #'evil-escape)

(define-key minibuffer-mode-map (kbd "C-j") 'next-line)
(define-key minibuffer-mode-map (kbd "C-k") 'previous-line)
(define-key minibuffer-mode-map (kbd "C-w") 'backward-kill-word)
