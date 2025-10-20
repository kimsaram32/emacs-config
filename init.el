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

;; Auto insert mode

(auto-insert-mode)

;; Note taking

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

(defconst me:note-day-start-hour 3 "The starting hour of a day for notes.")

(defun me:note-current-date ()
  "Return current date for the note system. If the time is before
`me:note-day-start-hour', return yesterday's date. Otherwise, return
today's date."
  (let* ((time (decode-time))
	 (current-day (decoded-time-day time))
	 (day (if (< (decoded-time-hour time) me:note-day-start-hour)
		  (- current-day 1)
		current-day)))
    (encode-time (append (list 0 0 0 day) (seq-subseq time 4)))))

(defun me:note-daily-name (date)
  "Return file name of the daily note for DATE."
  (expand-file-name
   (format-time-string "%Y/%m/%Y-%m-%d.org" date)
   me:note-periodic-directory))

(defun me:note-open-daily (date)
  "Open daily note for DATE in current window.

When called interactively, open daily note for the date returned by
`me:note-current-date'."
  (interactive (list (me:note-current-date)))
  (let* ((path (me:note-daily-name date))
	 (exists (file-exists-p path)))
    (find-file path)
    (unless exists
      (insert-file-contents (expand-file-name "daily.org" me:note-template-directory)))))

(keymap-global-set "C-c n d" 'me:note-open-daily)

;; ZK

(defun me:is-zettel-name-p (name)
  "Return t if NAME is a file name to a zettel. else return nill."
  (string-suffix-p "zk/" (file-name-directory name)))

(defun me:clear-zettels ()
  "Clear all Zettel buffers."
  (interactive)
  (mapc (lambda (x)
	  (if-let ((file-name (buffer-file-name x)))
	      (when (me:is-zettel-name-p file-name)
		(with-current-buffer x
		  (save-buffer)
		  (kill-buffer)))))
	(buffer-list)))

(keymap-global-set "C-c z c" 'me:clear-zettels)

(defun me:get-zettel-timestamp ()
  "Get zettel timestamp for current time."
  (format-time-string "%Y%m%d%H%M"))

(defun me:create-zettel ()
  "Create a Zettel."
  (interactive)
  (find-file (file-name-with-extension (expand-file-name (me:get-zettel-timestamp)
							 me:note-zk-directory)
				       ".org")))

(keymap-global-set "C-c z n" 'me:create-zettel)

(defun me:buffer-zettel-id ()
  "Return the zettel ID of current buffer."
  (interactive)
  (file-name-base (buffer-file-name)))

(defun me:buffer-zettel-id-as-link ()
  "Return the zettel ID of current buffer as Org link format."
  (interactive)
  (let ((id (me:buffer-zettel-id)))
    (format "[[zk:%s][%s]]" id id)))

(keymap-global-set "C-c z l" (lambda ()
			       (interactive)
			       (kill-new (me:buffer-zettel-id-as-link))))

(defun me:set-zettel-id-property ()
  "Set zettel ID as the value of 'custom_id' property to current buffer.

zettel ID is resolved by calling `me:buffer-zettel-id'."
  (interactive)
  (org-set-property "custom_id" (me:buffer-zettel-id)))

(defun me:zettel-search-backlinks ()
  "Search for ZK backlinks for current buffer via `rg'."
  (interactive)
  (rg-run (me:buffer-zettel-id) "*" me:note-zk-directory t))

(keymap-global-set "C-c z b" 'me:zettel-search-backlinks)

(defun me:set-date-properties ()
  "Update the 'updated' property of current buffer as current date.
Also update the 'created' property if one does not exist."
  (interactive)
  (let ((date-string (format-time-string "%Y-%m-%d" (me:note-current-date))))
    (save-excursion
      (unless (org-entry-get (point) "created")
	(org-set-property "created" date-string))
      (org-set-property "updated" date-string))))

(defun me:set-zettel-properties ()
  "Call `me:set-date-properties' and `me:set-zettel-id-property'."
  (interactive)
  (me:set-date-properties)
  (me:set-zettel-id-property))

(keymap-global-set "C-c z u" 'me:set-zettel-properties)

;; Markdown migration

(defvar me:pandoc-migration-command
  "pandoc --wrap=preserve -f commonmark+task_lists+yaml_metadata_block -s -t org --lua-filter=/Users/kimsaram32/me/myself/pandoc-filter.lua"
  "Command to use for `me:migrate-to-org'.") ; Hardcoded Lua filter path could be improved

(defun is-empty-line-p ()
  "Return t if current line only includes whitespace, else return
nil."
  (not (not (string-match-p "^[[:blank:]]*$"
        (buffer-substring (line-beginning-position)
                          (line-end-position))))))

(defun me:verbatim-to-code ()
  (interactive)
  (replace-regexp "=" "~"))

(keymap-global-set "C-c m c" 'me:verbatim-to-code)

(defun me:migrate-org-properties ()
  "Migrate current old Org document to follow the new format. Specifically:

- move incorrect buffer options (created and updated) into properties.
- if ZK, call `me:set-zettel-id-property'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((created-at (and (re-search-forward "^#\\+created: \\(.+\\)$" nil t)
			   (let ((val (match-string 1)))
			     (beginning-of-line)
			     (delete-line)
			     val)))
	  (updated-at (and (re-search-forward "^#\\+updated: \\(.+\\)$" nil t)
			   (let ((val (match-string 1)))
			     (beginning-of-line)
			     (delete-line)
			     val))))
      (org-next-visible-heading 1)
      (when created-at (org-set-property "created" created-at))
      (when updated-at (org-set-property "updated" updated-at)))
    (me:set-zettel-id-property)))

(defun me:migrate-markdown-to-org ()
  "Convert current markdown file into Org Mode document, using Pandoc.
Open the converted document in current buffer.

Perform the following transformations:

- move frontmatters (created and updated) into properties.
- if ZK, transform links and add custom_id property.

The original file is deleted."
  (interactive)
  (let* ((input-file-name (buffer-file-name))
	 (output-file-name (file-name-with-extension input-file-name ".org"))
	 (created-at)
	 (updated-at)
	 (is-zk (me:is-zettel-name-p input-file-name)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^created: \\(.+\\)$" nil t)
	(setq created-at (match-string 1)))
      (when (re-search-forward "^updated: \\(.+\\)$" nil t)
	(setq updated-at (match-string 1))))
    
    (find-file output-file-name)
    (call-process-shell-command me:pandoc-migration-command input-file-name t)

    (goto-char (point-min))
    (when is-zk
      (let ((tags))
	(while (re-search-forward "#\\([A-z0-9가-힣\\-]+\\)" nil t)
	  (push (match-string 1) tags)
	  (let ((start (match-beginning 0))
		(end (match-end 0)))
	    (delete-region start end)
	    (goto-char start)
	    (beginning-of-line)
	    (when (looking-at "^[[:space:]]*$")
	      (delete-line))))

	(insert (format "#+filetags: %s\n" (org-make-tag-string tags)))))

    (goto-char (point-min))
    (org-next-visible-heading 1)
    (when created-at
      (org-set-property "created" created-at))
    (when updated-at
      (org-set-property "updated" updated-at))
    
    (when is-zk
      (me:set-zettel-id-property)
      (save-excursion
	(replace-regexp "\\[\\[\\([0-9]+\\)]]" "[[zk:\\1][\\1]]")))
    
    (delete-file input-file-name)
    (kill-buffer (buffer-name (previous-buffer)))))

(defun me:migrate-to-org ()
  "Migrate current file. DWIM."
  (interactive)
  (let ((ext (file-name-extension (buffer-file-name))))
    (when (string-equal ext "md")
      (me:migrate-markdown-to-org))
    (when (string-equal ext "org")
      (me:migrate-org-properties))))

(keymap-global-set "C-c m m" 'me:migrate-to-org)

;; ORG mode

(defun me:resolve-zk-link (tag)
  "Resolve zk/TAG.org and fall back to zk/TAG.md."
  (let* ((base-name (expand-file-name tag me:note-zk-directory))
	 (org-name (concat base-name ".org"))
	 (md-name (concat base-name ".md")))
    (if (or (find-buffer-visiting org-name)
	    (file-exists-p org-name))
	org-name
      md-name)))

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
  (plist-put org-format-latex-options :scale 1.5)

  (setq org-link-abbrev-alist
	'(("zk" . "%(me:resolve-zk-link)")))
  (add-to-list 'org-file-apps '("\\.md\\'" . emacs))
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-agenda-files (list me:note-root-directory
			       (expand-file-name "buffers/" me:note-root-directory)
			       me:note-zk-directory))
  (setq org-agenda-window-setup 'current-buffer)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-todo-ignore-deadlines -1) ;; ignore past entries (older than today)
  
  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "|" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . "gold") ("IDEA" . "coral")
	  ("WORKING" . "CadetBlue2")))

  (setq org-priority-lowest 68
	org-priority-highest 65
	org-priority-default 68)
  (setq org-capture-templates
	`(("r" "Random thought"
	   plain (function ,(lambda ()
			      (me:note-open-daily (me:note-current-date))
			      (goto-char (point-max))))
	   "*** %<%H:%M> %?"
	   :empty-lines-before 1)
	  ("f" "Fleeting note"
	   entry (file ,(expand-file-name "fleeting.org" me:note-root-directory))
	   "* IDEA %?"
	   :empty-lines-before 1)
	  ("T" "Topic"
	   entry (file ,(expand-file-name "fleeting.org" me:note-root-directory))
	   "* TOPIC %?")
	  ("t" "Todo item"
	   entry (file ,(expand-file-name "todo.org" me:note-root-directory))
	   "* TODO %?"
	   :prepend t
	   :empty-lines-after 1)))
  (put 'me:resolve-zk-link 'org-link-abbrev-safe t))

;; Rg

(use-package rg :ensure t
  :bind (:map rg-mode-map
	      ("C-n" . rg-next-file)
	      ("C-p" . rg-prev-file))
  :config
  (rg-enable-default-bindings)
  (rg-define-search zettelkasten
    "Search for zettels"
    :dir me:note-zk-directory
    :menu ("Search" "z" "Zettelkasten")))

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
