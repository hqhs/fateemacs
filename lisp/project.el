;; -*- lexical-binding: t -*-

;; Built into Emacs, no need for use-package or straight
(require 'project)

;; Configure project.el behavior
(setq project-switch-commands
      '((project-find-file "Find file" "f")
        (project-find-regexp "Find regexp" "g")
        (project-dired "Find directory" "d")
        (project-eshell "Eshell" "e")
        (project-compile "Compile" "c")))

;; By default, project.el searches for either .git, .hg, etc.
;; Add more root markers here if needed
(setq project-find-functions
      '(project-try-vc
        ;; Add any custom project root finding functions
        ))

;; Store recent projects in fate-cache-dir
(setq project-list-file (concat fate-cache-dir "projects"))

;; Bind keys in the project map (accessed via C-x p by default)
(define-key project-prefix-map (kbd "m") #'project-compile)
(define-key project-prefix-map (kbd "k") #'project-kill-buffers)

;; Optional: Configure project switching behavior
(setq project-switch-use-entire-frame t)  ; Use full frame for project commands
(setq project-kill-buffers-display-buffer-list t)  ; Show buffer list when killing

;; Optional: Integration with consult for better UI
(with-eval-after-load 'consult
  ;; Use consult-ripgrep instead of project-find-regexp when available
  (define-key project-prefix-map (kbd "g") #'consult-ripgrep)
  (evil-define-key '(normal visual) 'global
    (kbd "<leader>pg") #'consult-ripgrep))

;; Optional: Make project commands available in more buffers
(setq project-switch-commands-respect-buffer t)

;; Optional: Configure project VC integration
(setq project-vc-merge-submodules nil)  ; Don't descend into git submodules
(setq project-vc-ignores '(".git" ".hg" ".bzr" "_darcs"))

;; Optional: Dired integration
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c p") project-prefix-map))

(provide 'project-config)
