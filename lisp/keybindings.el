;; -*- lexical-binding: t -*-

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key '(normal visual) 'global
  (kbd "<leader>,") 'switch-to-buffer
  (kbd "<leader>.") 'find-file
  ;; magit
  (kbd "<leader>gg") #'magit-status
  ;; projectile
  (kbd "<leader><leader>") #'projectile-find-file
  (kbd "<leader>pp") #'projectile-switch-project

  ;; remappings
  (kbd "zx") 'kill-current-buffer)

(evil-define-key* '(insert replace visual operator) 'global
  (kbd "\C-g") #'evil-escape)

(define-key minibuffer-mode-map (kbd "C-j") 'next-line)
(define-key minibuffer-mode-map (kbd "C-k") 'previous-line)
(define-key minibuffer-mode-map (kbd "C-w") 'backward-kill-word)
