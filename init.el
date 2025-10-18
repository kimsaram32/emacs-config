;;; -*- lexical-binding: t -*-

;; Packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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

;; Tab bar
(tab-bar-mode)

(keymap-global-set "C-c <SPC>" tab-prefix-map)
(keymap-set tab-prefix-map "]" 'tab-next)
(keymap-set tab-prefix-map "[" 'tab-previous)

(setq tab-bar-auto-width-max '((140) 20))

;; Editing
(setq sentence-end-double-space nil)

(visual-line-mode)

(setq set-mark-command-repeat-pop t)

(setq initial-scratch-message "\
;; Hi this is a scratch buffer

")

(keymap-global-set "C-c b l" 'previous-buffer)
(keymap-global-set "C-c b r" 'next-buffer)

;;;;

(defun me:insert-quotes (&optional arg)
  "Like `insert-parentheses`, but for quotes."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(keymap-global-set "M-\"" 'me:insert-quotes)

;; YaSnippet

(use-package yasnippet :ensure t)

;;;;

(defconst me:note-root-directory "~/me/myself/"
  "Root directory for notes.")

(defconst me:note-periodic-directory
  (file-name-concat me:note-root-directory "life/")
  "Directory for periodic notes.")

(defconst me:note-zk-directory
  (file-name-concat me:note-root-directory "zk/")
  "Directory for Zettelkasten.")

(defconst me:note-template-directory
  (file-name-concat me:note-root-directory "_templates")
  "Directory for template notes.")

(defun me:note-daily-name (date)
  "Return file name of daily note for DATE."
  (expand-file-name
   (format-time-string "%Y/%m/%Y-%m-%d.org" (current-time))
   me:note-periodic-directory))

(defun me:note-open-daily (date)
  "Open daily note for DATE in current window.
When called interactively, open daily note for current date."
  (interactive (list (current-time)))
  (let* ((path (me:note-daily-name date))
	 (exists (file-exists-p path)))
    (find-file path)
    (unless exists
      (insert-file-contents (expand-file-name "daily.org" me:note-template-directory)))))

(keymap-global-set "C-c n d" 'me:note-open-daily)

;; ORG mode
(use-package org
  :init
  (setq org-use-extra-keys t)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 
	 :map org-mode-map
	 ("M-{" . org-backward-element)
	 ("M-}" . org-forward-element))
  :config
  (setq org-directory me:note-root-directory)
  
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item auto)))
  (setq org-preview-latex-default-process 'dvisvgm)
  
  (setq org-agenda-files (list me:note-root-directory
			       (expand-file-name "buffers/" me:note-root-directory)
			       (expand-file-name "zk/" me:note-root-directory)))
  (setq org-agenda-window-setup 'current-buffer)
  (setq org-agenda-restore-windows-after-quit t)
  
  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "|" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . "gold") ("IDEA" . "coral")
	("WORKING" . "CadetBlue2"))))

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
  (setq elfeed-search-filter "+unread")
  (load (expand-file-name "elfeed.el" user-emacs-directory))
  (elfeed-update))

;; Modus-themes

(use-package modus-themes :ensure t
  :init
  (require-theme 'modus-themes)
  :bind (("<f5>" . modus-themes-rotate))
  :config
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (modus-themes-load-theme 'modus-vivendi-tinted))

(put 'scroll-left 'disabled nil)
