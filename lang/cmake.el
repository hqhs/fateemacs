;; -*- lexical-binding: t -*-

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(provide 'cmake)
