;; -*- lexical-binding: t -*-

(setq gc-cons-threshold (* 100 1024 1024))

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
