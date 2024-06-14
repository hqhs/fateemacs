;; -*- lexical-binding: t -*-

(use-package cmake-mode
  :straight (cmake-mode :type git :host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(provide 'cmake)
