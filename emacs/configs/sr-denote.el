;;; sr-denote.el --- Personal configuration for Denote  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Minjeong Kim

;; Author: Minjeong Kim
;; URL: https://github.com/kimsaram32/dotfiles

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;;; Dependencies

(require 'denote)
(require 'denote-explore)
(require 'consult-denote)

;;; Keymap

(defvar-keymap sr/denote-map)

(keymap-global-set "C-c n" sr/denote-map)

(keymap-set sr/denote-map "n" #'sr/denote-create-note-zettelkasten)
(keymap-set sr/denote-map "s" #'sr/denote-create-note-second-brain)

(keymap-set sr/denote-map "r" #'denote-rename-file)
(keymap-set sr/denote-map "R" #'denote-rename-file-using-front-matter)

(keymap-set sr/denote-map "l l" #'denote-link)
(keymap-set sr/denote-map "l b" #'sr/consult-denote-backlinks)
(keymap-set sr/denote-map "l f" #'sr/denote-link-from-last-buffer)
(keymap-set sr/denote-map "l 4" #'sr/denote-link-from-other-window)
(keymap-set sr/denote-map "l w" #'sr/denote-save-link-to-kill-ring)
(keymap-set sr/denote-map "a" #'denote-find-link)

(keymap-set sr/denote-map "d" #'denote-dired)
(keymap-set sr/denote-map "f" #'sr/denote-find-note)
(keymap-set sr/denote-map "A" #'sr/denote-find-all-files)

(keymap-set sr/denote-map "g" #'consult-denote-grep)

(keymap-set sr/denote-map "e r" #'sr/denote-explore-random-zettel)

(keymap-set sr/denote-map "m" #'sr/denote-move-org-entries-dwim)

;;; Basic configuration

(defconst sr/denote-primary-directories
  (list sr/note-zk-directory sr/note-second-brain-directory sr/note-periodic-directory)
  "Primary directories for Denote.")

(defconst sr/denote-secondary-directories
  (list "~/me/z")
  "Secondary directories for Denote.")

(setq denote-directory (append sr/denote-primary-directories sr/denote-secondary-directories))
(setq denote-prompts '(title keywords subdirectory))
(setq denote-save-buffers t)

(setq denote-open-link-function #'find-file)

;;; Linking

(defun sr/denote-link-from-last-buffer (id-only)
  "Create link to the last visited file buffer.
ID-ONLY is the same as `denote-link'."
  (interactive "P")
  (declare (interactive-only t))
  (let ((prev-buffers (window-prev-buffers (selected-window))))
    (while (not (buffer-file-name (car (car prev-buffers))))
      (unless (setq prev-buffers (cdr prev-buffers))
        (user-error "No file-visiting buffer found")))
    (let* ((file (buffer-file-name (car (car prev-buffers))))
           (file-type (denote-filetype-heuristics buffer-file-name))
           (description (denote-get-link-description file)))
      (denote-link file file-type description id-only))))

(defun sr/denote-link-from-other-window (id-only)
  "Create link to the file buffer in the other window.
ID-ONLY is the same as `denote-link'."
  (interactive "P")
  (declare (interactive-only t))
  (if-let* ((window (next-window))
            (buffer (window-buffer window))
            (file (buffer-file-name buffer))
            (file-type (denote-filetype-heuristics buffer-file-name))
            (description (denote-get-link-description file)))
      (denote-link file file-type description id-only)
    (user-error "No file-visiting buffer found")))

(defun sr/denote-save-link-to-kill-ring (id-only)
  "Save the link pointing the current note to the kill ring.
It infers the link format using the file type of the current buffer, so
this will break across different file formats."
  (interactive "P")
  (declare (interactive-only t))
  (if-let* ((file buffer-file-name)
            (file-type (denote-filetype-heuristics file))
            (description (denote-get-link-description file))
            (link (denote-format-link file description file-type id-only)))
      (progn
        (kill-new link)
        (message link))
    (user-error "could not create link: check the buffer is visitng a file.")))

(defun sr/denote-print-title-after-link (file file-type description &optional id-only)
  "Print the title of the linked note.
This function is intended to be used as advice for `denote-link'."
  (message (format "Linked: \"%s\"" (denote-retrieve-title-or-filename file file-type))))

(advice-add 'denote-link :after #'sr/denote-print-title-after-link)

;;; Note creation

(defun sr/denote-get-initial-data-dwim ()
  "Get appopriate DWIM initial data for new notes.
This returns a plist of two properties: TITLE and CONTENT."
  (pcase major-mode
    ('eww-mode
     (let ((url (eww-current-url))
           (title (or (plist-get eww-data :title) url)))
       (list
        :title title
        :content (format "[[%s][%s]]" url title))))))

(defun sr/denote-create-note-second-brain ()
  "Create a new note for the second brain."
  (declare (interactive-only t))
  (interactive)
  (let* ((initial-data (sr/denote-get-initial-data-dwim))
         (denote-use-title (plist-get initial-data :title)))
    (call-interactively 'denote)
    (when-let ((content (plist-get initial-data :content)))
      (insert content))))

(defun sr/denote-create-note-problem ()
  "Create a new problem note."
  (declare (interactive-only t))
  (interactive)
  (let* ((section (read-from-minibuffer "Section number (e.g. 1-5): "))
         (page (read-number "Page number: "))
         (problem-num (read-number "Problem number: "))
         (comment (read-from-minibuffer "Comment (optional): "))

         (denote-use-signature "sb")
         (denote-use-title
          (format
           "Problem (s)%s %dp - %d%s"
           section page problem-num
           (if (not (string-empty-p comment))
               (format " (%s)" comment)
             "")))
         (denote-use-keywords
          (append
           (denote-keywords-prompt)
           '("problem"))))
    (call-interactively 'denote)))

(defun sr/denote-create-note-zettelkasten ()
  "Create a new note for zettelkasten."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-use-directory sr/note-zk-directory))
    (call-interactively 'denote)))

;;; Other commands

(defun sr/denote-find-note ()
  (interactive)
  (let ((denote-directory sr/denote-primary-directories))
    (find-file (denote-file-prompt))))

(defun sr/denote-find-all-files ()
  (interactive)
  (find-file (denote-file-prompt)))

(defun sr/denote-explore-random-zettel ()
  (interactive)
  (let ((denote-directory sr/note-zk-directory))
    (denote-explore-random-note)))

;;; Buffer integration

(setq denote-buffer-name-prefix "d: ")
(setq denote-rename-buffer-format "%i %t")
(denote-rename-buffer-mode 1)

(defun sr/denote-rename-after-save ()
  "Rename the current note if needed.
This function is intended to be added to `after-save-hook'."
  (let ((denote-rename-confirmations nil))
    (when (and buffer-file-name (denote-file-has-denoted-filename-p buffer-file-name))
      (ignore-errors (denote-rename-file-using-front-matter buffer-file-name)))))

(add-hook 'after-save-hook #'sr/denote-rename-after-save)

;;; Dired integration

(add-hook 'dired-mode-hook #'denote-dired-mode)

(defun sr/dired-rename-update-denote-front-matter (file new-name ok-if-already-exists)
  "After renaming files in Dired, update Denote front matter if appropriate."
  (when (and (denote-file-has-denoted-filename-p file)
             (denote-file-has-denoted-filename-p new-name)
             denote-rename-rewrite-front-matter
             (denote-file-is-writable-and-supported-p new-name))
    (if (not (equal (denote-retrieve-filename-title file)
                    (denote-retrieve-filename-title new-name)))
        (message "Title change for '%s' is ignored" new-name))
    (when-let* ((file-type (denote-filetype-heuristics new-name))
                (title (denote-retrieve-title-or-filename new-name file-type))
                (keywords (denote-keywords-sort
                           (denote-retrieve-filename-keywords-as-list new-name)))
                (signature (or (denote-retrieve-filename-signature new-name) ""))
                (date (denote-valid-date-p (denote-retrieve-filename-identifier new-name)))
                (identifier (denote-retrieve-filename-identifier new-name)))
      (if (denote--file-has-front-matter-p new-name file-type)
          (denote-rewrite-front-matter new-name title keywords signature date identifier file-type)
        (when (denote-add-front-matter-prompt new-name)
          (denote-prepend-front-matter new-name title keywords signature date identifier file-type))))))

(advice-add 'dired-rename-file :after #'sr/dired-rename-update-denote-front-matter)

;;; Text mode integration

(add-hook 'text-mode-hook #'denote-fontify-links-mode)

;;; Consult integration

(consult-denote-mode)

(defun sr/consult-denote-buffer ()
  (interactive)
  (consult-buffer (list consult-denote-buffer-source)))

(defun sr/consult-denote-backlinks ()
  "Like `denote-backlinks', but uses `consult-xref' for displaying."
  (interactive)
  (if-let* ((file buffer-file-name))
      (if-let* ((identifier (denote-retrieve-filename-identifier file))
                (fetcher (lambda ()
                           (let* ((denote-alist (denote-retrieve-xref-alist-for-backlinks identifier))
                                  (xrefs (mapcan #'cdr denote-alist)))
                             xrefs))))
          (consult-xref fetcher)
        (user-error "The current file does not have a Denote identifier"))
    (user-error "Buffer `%s' is not associated with a file" (current-buffer))))

;;; Migration from entry-based workflow w/ Org

;; I want the created notes to be in my second brain.
(defvar sr/denote-signature-for-org-entry "sb"
  "Signature to use for Denote notes from Org mode entries.")

(defvar sr/denote-directory-for-org-entry
  sr/note-second-brain-directory
  "Directory for Denote notes from Org mode entries.")

(defun sr/org-entry-content-start ()
  "Return the position just after the heading of the current entry."
  ;; this works based on the assumption that the heading is one line
  ;; long.
  (save-excursion
    (let ((end (org-element-end (org-element-at-point))))
      (while (progn
               (forward-line 1)
               ;; handle empty entries.
               (and (not (eobp))
                    (<= (point) end)
                    (looking-at "^$"))))
      (min (point) end))))

(defconst sr/org-simple-link-regexp
  "\\[\\[\\([^]\n]+\\)\\]\\(\\[\\([^]\n]+\\)\\]\\)?\\]"
  "Simple regexp to match Org mode links.")

(defun sr/org-remove-links (str)
  "Remove all Org links from STR.
Return a cons cell whose car is the resulting string and cdr is the
extracted links."
  (let ((links '())
        (pos 0))
    (while (and (< pos (length str))
                (string-match sr/org-simple-link-regexp str pos))
      (let* ((full (match-string 0 str))
             (link (match-string 1 str))
             (desc (or (match-string 3 str) link)))
        (push full links)
        (setq str (replace-match desc t t str))
        (setq pos (1+ (match-end 0)))))
    (cons str (nreverse links))))

(defun sr/denote-get-date-from-org-entry ()
  "Extract the date to use in Denote notes from current Org entry.
  Returned date is the string in the format of `org-timestamp-formats'.
  Return nil if the date cannot be found."
  (save-match-data
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (if (re-search-forward
           (org-re-timestamp 'active)
           ;; `org-entry-end-position' does not include subtrees,
           ;; which is required here.
           (org-entry-end-position)
           t)
          (match-string 1)
        nil))))

(defun sr/denote-new-note-from-org-entry ()
  "Create a new note from the Org entry at point."
  (interactive "P")
  (let* ((element (save-excursion
                    (org-back-to-heading t)
                    (org-element-at-point)))
         (start (org-element-begin element))
         (end (org-element-end element))

         (content-start (sr/org-entry-content-start))
         (content (buffer-substring content-start end))

         (denote-use-directory sr/denote-directory-for-org-entry)
         (denote-use-signature sr/denote-signature-for-org-entry)
         denote-use-title
         denote-use-keywords
         denote-use-date
         links)

    ;; extract metadata from the entry.
    (save-excursion
      (goto-char start)
      (let ((extracted (sr/org-remove-links (org-get-heading t t t t))))
        (setq denote-use-title (car extracted))
        (setq links (cdr extracted)))
      (setq denote-use-keywords
            (denote-sluggify-keywords-and-apply-rules (org-get-tags)))
      (setq denote-use-date (sr/denote-get-date-from-org-entry)))

    (message (format "title: %s" denote-use-title))

    (save-current-buffer
      (call-interactively 'denote)

      ;; insert links.
      (insert (string-join links "\n"))
      (when links (insert "\n\n"))

      (insert content)

      ;; correct the heading levels.
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "^\\*\\*" nil t)
          (replace-match "*"))))))

(defun sr/outline-try-forward-same-level-p (n)
  "Try `outline-forward-same-level' and return t on success.
  Return nil otherwise."
  (condition-case nil
      (progn
        (outline-forward-same-level n)
        t)
    (error nil)))

(defun sr/denote-move-org-entries-in-buffer (preserve)
  "Move all Org entries in the current buffer to new notes.
  Delete the original entries unless PRESERVE is non-nil."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (unless (outline-on-heading-p t)
      (outline-next-heading))
    (while (progn
             (sr/denote-new-note-from-org-entry)
             (sr/outline-try-forward-same-level-p 1)))
    (unless preserve
      (delete-region (point-min) (point-max)))))

(defun sr/denote-move-org-entries-in-region (preserve)
  "Move all Org entries in the region to new notes.
  Delete the original entries unless PRESERVE is non-nil."
  (interactive "P")
  (unless (region-active-p)
    (user-error "Region is not active"))
  (with-restriction (region-beginning) (region-end)
    (sr/denote-move-org-entries-in-buffer preserve)))

(defun sr/denote-move-org-entries-dwim (preserve)
  "Move Org entries to new notes in the current context.
  This is a simple wrapper around `sr/denote-move-org-entries-in-region' and `sr/denote-move-org-entries-in-buffer'."
  (declare (interactive-only t))
  (interactive "P")
  (if (region-active-p)
      (sr/denote-move-org-entries-in-region preserve)
    (sr/denote-move-org-entries-in-buffer preserve)))

;;; Performance improvement hacks

(defvar sr/denote--directory-files-cache nil)
(defvar sr/denote--directory-files-use-cache nil)

(defun sr/org-link-preview-flag (orig &rest args)
  (setq sr/denote--directory-files-use-cache t)
  (apply orig args)
  (setq sr/denote--directory-files-use-cache nil))

(defun sr/denote--directory-files-with-cache (orig &rest args)
  (if (and sr/denote--directory-files-use-cache
           sr/denote--directory-files-cache)
      sr/denote--directory-files-cache
    (setq sr/denote--directory-files-cache (apply orig args))))

(advice-add 'org-link-preview :around #'sr/org-link-preview-flag)
(advice-add 'denote-directory-files :around #'sr/denote--directory-files-with-cache)

;;; _

(provide 'sr-denote)

;;; sr-denote.el ends here
