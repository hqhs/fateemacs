;; -*- lexical-binding: t -*-

;; TODO: rewrite as custom keyword mode
(when (locate-library "meson-mode")
  (use-package meson-mode
    :mode (("\\.meson\\'" . meson-mode)
           ("meson\\.build\\'" . meson-mode)
           ("meson_options\\.txt\\'" . meson-mode))
    :hook ((meson-mode . eldoc-mode))
    :config
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

    :custom
    (meson-indent-basic 4)
    (meson-indent-level 4)))
