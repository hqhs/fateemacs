;; -*- lexical-binding: t -*-

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key '(normal visual operator) 'global
  (kbd "<leader>,") #'consult-buffer
  (kbd "<leader>.") #'find-file
  ;; search
  (kbd "<leader>si") #'consult-imenu
  (kbd "<leader>sp") #'consult-ripgrep
  ;; magit
  (kbd "<leader>gg") #'magit-status
  (kbd "<leader>gb") #'magit-blame
  ;; projectile
  (kbd "<leader>SPC")      #'projectile-find-file
  (kbd "<leader>pp")       #'projectile-switch-project
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
