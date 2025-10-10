;;; -*- lexical-binding: t -*-

;; Melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Font
; Prevent resizing windows when font changes.
(setq-default window-size-fixed t)

(set-face-attribute 'default nil
		    :height 150
		    :family "JetBrains Mono")

;; Menu bar
(menu-bar-mode -1)

;; Tool bar
(set-frame-parameter nil 'ns-transparent-toolbar t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(tool-bar-mode -1)
(setq-default frame-title-format "")

;; Scroll bar
(scroll-bar-mode -1)

;; Make sentences end with a single space.
(setq sentence-end-double-space nil)

;; Enable visual line mode.
(visual-line-mode)

(setq set-mark-command-repeat-pop t)

;; Org mode
(use-package org
  :bind (:map org-mode-map
	 ("M-{" . org-backward-element)
	 ("M-}" . org-forward-element)
	 ("M-p" . org-metaleft)
	 ("M-n" . org-metaright)))

;; CC Mode
(setq c-default-style "bsd")

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq c-basic-offset 2)))
