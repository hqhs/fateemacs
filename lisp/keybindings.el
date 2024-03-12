;; -*- lexical-binding: t -*-

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key '(normal visual) 'global
  (kbd "<leader>,") 'switch-to-buffer
  (kbd "<leader>.") 'find-file
  ;; magit
  (kbd "<leader>gg") #'magit-status
  ;; projectile
  (kbd "<leader>pp") #'projectile-switch-project)
