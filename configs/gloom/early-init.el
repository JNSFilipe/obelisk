;;; early-init.el --- Standalone startup -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil
      gc-cons-threshold (* 100 1024 1024)
      frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
