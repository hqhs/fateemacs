;; -*- lexical-binding: t -*-

;; PERF: Defer GC entirely during startup, reset after init
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; PERF: Suspend file-name-handler-alist during startup (speeds up all file I/O)
(defvar +fate--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; PERF: Suppress mode-line recalculation during startup (it runs 20+ times)
(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))

;; PERF: Suppress redisplay until we're ready
(setq-default inhibit-redisplay t)
(setq-default inhibit-message t)

;; Restore everything on first user command
(defun +fate--reset-startup-optimizations-h ()
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  ;; Restore mode-line
  (setq-default mode-line-format (get 'mode-line-format 'initial-value))
  (remove-hook 'post-command-hook #'+fate--reset-startup-optimizations-h))
(add-hook 'post-command-hook #'+fate--reset-startup-optimizations-h -100)

;; Restore file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist +fate--file-name-handler-alist)
            ;; Reset GC to reasonable runtime values
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16mb
                  gc-cons-percentage 0.1))
          101)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
