;; -*- lexical-binding: t -*-

(use-package meson-mode
  :ensure t
  :straight t
  :mode (("\\.meson\\'" . meson-mode)
         ("meson\\.build\\'" . meson-mode)
         ("meson_options\\.txt\\'" . meson-mode))
  :hook ((meson-mode . company-mode)
         (meson-mode . eldoc-mode))
  :config
  ;; Set up compilation commands
  (defun meson-build ()
    "Build using Meson"
    (interactive)
    (compile "meson compile -C build"))

  (defun meson-configure ()
    "Configure Meson project"
    (interactive)
    (compile "meson setup build"))

  (defun meson-test ()
    "Run Meson tests"
    (interactive)
    (compile "meson test -C build"))

  (defun meson-reconfigure ()
    "Reconfigure Meson project"
    (interactive)
    (compile "meson setup --reconfigure build"))

  ;; Key bindings for Meson commands
  ;; :bind (:map meson-mode-map
  ;;        ("C-c m b" . meson-build)
  ;;        ("C-c m c" . meson-configure)
  ;;        ("C-c m t" . meson-test)
  ;;        ("C-c m r" . meson-reconfigure))

  :custom
  (meson-indent-basic 4)
  (meson-indent-level 4))

;; Optional: Add project-specific compilation commands with projectile
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (projectile-register-project-type 'meson '("meson.build")
;;                                    :compile "meson compile -C build"
;;                                    :test "meson test -C build"
;;                                    :configure "meson setup build"
;;                                    :package "meson dist"))
