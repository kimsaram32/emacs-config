;;; -*- lexical-binding: t -*-

(defun me:expand-emacs-file-name (name)
  "Resolve NAME relative to `user-emacs-directory`."
  (expand-file-name name user-emacs-directory))

;; Melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Custom
(setq custom-file (me:expand-emacs-file-name "custom.el"))
(load custom-file)

;; Font
; Prevent resizing windows when font changes.
(setq-default window-size-fixed t)
(set-face-attribute 'default nil
		    :height 150
		    :family "JetBrains Mono")
(setq-default window-size-fixed nil)

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

;; Make C-SPC after C-u C-SPC traverse the mark ring.
(setq set-mark-command-repeat-pop t)

;; Scratch buffer
(setq initial-scratch-message "\
;; Hi this is a scratch buffer

")

;;;;

(defun me:insert-quotes (&optional arg)
  "Like `insert-parentheses`, but for quotes."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(keymap-global-set "M-\"" 'me:insert-quotes)

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

;; Elfeed
(defun me:elfeed-show-untag-unread ()
  (interactive)
  (elfeed-show-untag 'unread))

(use-package elfeed :ensure t
  :bind (("C-x w" . 'elfeed)
	 :map elfeed-search-mode-map
	 ("TAB" . next-line)
	 ("S-<tab>" . previous-line)
	 :map elfeed-show-mode-map
	 ("r" . me:elfeed-show-untag-unread))
  :config
  (load (me:expand-emacs-file-name "elfeed.el"))
  (elfeed-update))

;; Modus-themes

(use-package modus-themes :ensure t
  :init
  (require-theme 'modus-themes)
  :bind (("<f5>" . modus-themes-rotate))
  :config
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (modus-themes-load-theme 'modus-vivendi-tinted))
