;; -*- lexical-binding: t -*-

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . +fate--rust-maybe-eglot))
  :config

  (defun +fate--rust-maybe-eglot ()
    "Start eglot only if the file belongs to a Cargo project."
    (when (locate-dominating-file default-directory "Cargo.toml")
      (eglot-ensure)))

  (add-hook 'rust-ts-mode-hook
            (lambda ()
              (setq-local +fate-format-command
                          '("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout"))))

  (add-to-list 'compilation-error-regexp-alist 'rust-cargo)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rust-cargo
                 "^\\s-*-->\\s-*\\([^\n:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  (defun rust-format-buffer ()
    "Format the current Rust project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (compile "cargo fmt --all")))

  (defun rust-clippy ()
    "Run clippy on the current Rust project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (compile "cargo clippy --all-targets --all-features -- -D warnings")))

  (defun rust-test ()
    "Run tests for the current Rust project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (compile "cargo test --all-features")))

  (evil-define-key 'normal rust-ts-mode-map
    (kbd "SPC m f") 'rust-format-buffer
    (kbd "SPC m c") 'rust-clippy
    (kbd "SPC m t") 'rust-test))
