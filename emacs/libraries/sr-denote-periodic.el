;;; sr-denote-periodic.el --- Periodic notes in Denote  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Minjeong Kim

;; Author: Minjeong Kim <kimsaram32@fastmail.com>
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

;; Manage periodic notes in Denote.
;;
;; A periodic note is a note associated to a specific period, e.g. a day, a
;; week, a month, etc.
;;
;; The idea is establishing a unique identifier encoding format for each period,
;; e.g, "D20260826" for daily notes, "W202633" for weekly notes, and so on. This
;; guarantees that at most one note is associated to a period.
;;
;; The encoding formats, as well as additional metadata for creation of new
;; periodic notes, are stored in `sr/denote-periodic-types'.
;;
;; Notes are located in a separate subdirectory for each year, e.g., notes in
;; 2026 are inside "2026/".

;;; Code:

(require 'denote)

;;; Note types

(defgroup sr/denote-periodic nil
  "Periodic notes in Denote."
  :group 'files
  :group 'denote)

(defcustom sr/denote-periodic-directory
  "~/periodic"
  "Directory for storing periodic notes."
  :type 'directory)

(defcustom sr/denote-periodic-note-keywords
  '("periodic")
  "List of Denote keywords added to periodic notes."
  :type '(list string))

(defcustom sr/denote-periodic-types
  '((daily
     :id-format "D%Y%m%d"
     :regexp "D\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
     :title-format "%Y-%m-%d"
     :signature "daily")

    (monthly
     :id-format "M%Y%m"
     :regexp "M\\([0-9]\\{6\\}\\)"
     :title-format "%Y-%m"
     :signature "monthly")

    (yearly
     :id-format "Y%Y"
     :regexp "Y\\([0-9]\\{4\\}\\)"
     :title-format "%Y"
     :signature "yearly")

    (weekly
     :id-format "W%Y%W"
     :regexp "W\\([0-9]\\{6\\}\\)"
     :title-format "%Y-W%W"
     :signature "weekly"))
  "Alist of periodic note types.

Each element is of the form (NAME . PLIST). NAME is a symbol for the
note type. PLIST is a plist that consists of the following elements:

- `:id-format' is a format string (see `format-time-string') that is used
  to generate a note identifier.

- `:regexp' is a regular expression used to retrieve the note identifier
  from a file name.

- `:title-format' is a format string that is used to construct the title
  of a new note.

- `:signature' is a Denote signature to use in new notes.

Changing the value of this variable does not create or delete the
convenience \"today\" functions, e.g.,
`sr/denote-periodic-daily-note-today'."
  :type '(alist :key-type symbol
                :value-type plist))

(defcustom sr/denote-periodic-get-today-date-function
  #'current-time
  "Function to get the date of today.
The function should return a time value."
  :type 'function)

(defun sr/denote-periodic-get-type (type)
  "Get the plist of periodic note type TYPE."
  (cdr (assq type sr/denote-periodic-types)))

(defun sr/denote-periodic-date-to-identifier (date type)
  "Generate a note identifier for TYPE corresponding to DATE.
DATE should be a time value."
  (format-time-string
   (plist-get (sr/denote-periodic-get-type type) :id-format)
   date))

;;; Note creation

(defun sr/denote-periodic-create-note (date type)
  "Create the periodic note of TYPE for DATE.
This function should be called only if there is no existing note with
the encoded identifier."
  (let ((type-entry (sr/denote-periodic-get-type type))
        (directory (expand-file-name
                    (format-time-string "%Y" date)
                    sr/denote-periodic-directory)))
    (unless (file-exists-p directory)
      (make-directory directory))
    (denote
     (format-time-string (plist-get type-entry :title-format) date)
     sr/denote-periodic-note-keywords
     nil
     directory
     date
     nil
     (plist-get type-entry :signature)
     (sr/denote-periodic-date-to-identifier date type))))

(defun sr/denote-periodic--calendar-date-to-time (date)
  "Convert a calendar date to a time value."
  (encode-time
   (decoded-time-set-defaults
    (list
     nil nil nil
     (calendar-extract-day date)
     (calendar-extract-month date)
     (calendar-extract-year date)
     nil -1 nil))))

(defun sr/denote-periodic-time-prompt ()
  "Prompt for a time value."
  (sr/denote-periodic--calendar-date-to-time (calendar-read-date)))

(defun sr/denote-periodic-type-prompt ()
  "Prompt for periodic type."
  (intern (completing-read
           "Period type: " (mapcar #'car sr/denote-periodic-types)
           nil t)))

;;;###autoload
(defun sr/denote-periodic-find-or-create-note (date type &optional prompt)
  "Find the periodic note of TYPE for DATE, or create one if it does not exist.
When PROMPT is non-nil, ask the user to confirm the creation; otherwise,
create the note without confirmation."
  (interactive (let ((time (sr/denote-periodic-time-prompt))
                     (type (sr/denote-periodic-type-prompt)))
                 (list time type
                       (format "No %s note for %s. Create?"
                               type (format-time-string "%Y-%m-%d" time)))))
  (if-let* ((file (denote-get-path-by-id
                   (sr/denote-periodic-date-to-identifier date type))))
      (find-file file)
    (when (or (not prompt) (y-or-n-p prompt))
      (sr/denote-periodic-create-note date type))))

;;;; Convenience functions

;;;###autoload
(defun sr/denote-periodic-today (type)
  "Find or create the periodic note of TYPE for the current date."
  (interactive (list (sr/denote-periodic-type-prompt)))
  (let ((date (funcall sr/denote-periodic-get-today-date-function)))
    (sr/denote-periodic-find-or-create-note
     date type
     (format "No %s note for. Create?" type))))

(defmacro sr/denote-periodic-define-today-function (name)
  `(defun ,(intern (format "sr/denote-periodic-%s-note-today" name)) ()
     ,(format "Find or create the %s note for the current date." name)
     (interactive)
     (sr/denote-periodic-today ',name)))

(sr/denote-periodic-define-today-function daily)
(sr/denote-periodic-define-today-function weekly)
(sr/denote-periodic-define-today-function monthly)
(sr/denote-periodic-define-today-function yearly)

;;; Daily note mode

;; TODO: What if the user customizes the daily note type?

(defun sr/denote-periodic-daily-note-id-to-date (identifier)
  "Return the corresponding date of a daily note identifier IDENTIFIER.
if IDENTIFIER is invalid as a daily note identifier, return nil."
  (when (string-match (plist-get (sr/denote-periodic-get-type 'daily) :regexp) identifier)
    ;; (SECOND MINUTE HOUR DAY MONTH YEAR)
    (encode-time 0 0 0
                 (string-to-number (match-string 3 identifier))
                 (string-to-number (match-string 2 identifier))
                 (string-to-number (match-string 1 identifier)))))

(defun sr/denote-periodic-daily-note-buffer-date ()
  (and-let* ((file (buffer-file-name))
             (identifier (denote-retrieve-filename-identifier file)))
    (sr/denote-periodic-daily-note-id-to-date identifier)))

;;;###autoload
(defun sr/denote-periodic-find-previous-daily-note ()
  "Find the previous daily note."
  (interactive)
  (if-let ((date (sr/denote-periodic-daily-note-buffer-date)))
      (sr/denote-periodic-find-or-create-note
       (time-subtract date (days-to-time 1))
       'daily
       "No previous daily note. create?")
    (user-error "Not inside a daily note")))

;;;###autoload
(defun sr/denote-periodic-find-next-daily-note ()
  "Find the next daily note."
  (interactive)
  (if-let ((date (sr/denote-periodic-daily-note-buffer-date)))
      (sr/denote-periodic-find-or-create-note
       (time-add date (days-to-time 1))
       'daily
       "No next daily note. create?")
    (user-error "Not inside a daily note")))

(defvar-keymap sr/denote-periodic-daily-note-mode-map
  "C-c j n" #'sr/denote-periodic-find-next-daily-note
  "C-c j p" #'sr/denote-periodic-find-previous-daily-note)

;;;###autoload
(define-minor-mode sr/denote-periodic-daily-note-mode
  "Minor mode for daily notes.")

;;; Calendar integration (in progress)

;; TODO
(defun sr/denote-periodic-daily-note-id-from-calendar (date)
  (format
   "D%d%02d%02d"
   (calendar-extract-year date)
   (calendar-extract-month date)
   (calendar-extract-day date)))

(defun sr/denote-periodic-calendar-find-daily-note-at-cursor ()
  (interactive)
  (if-let* ((identifier (sr/denote-periodic-daily-note-id-from-calendar (calendar-cursor-to-date)))
            (file (denote-get-path-by-id identifier)))
      (funcall denote-open-link-function file)
    (user-error "No daily note for date at point")))

(defvar-keymap sr/denote-periodic-calendar-mode-map
  "d" #'sr/denote-periodic-calendar-find-daily-note-at-cursor)

;;;###autoload
(define-minor-mode sr/denote-periodic-calendar-mode
  "Minor mode for navigating periodic notes within `calendar'.

Currently, it only supports opending a daily note at point, if any."
  :lighter " Periodic"
  :global nil)

(add-hook 'calendar-mode-hook #'sr/denote-periodic-calendar-mode)

;;; _

(provide 'sr-denote-periodic)

;;; sr-denote-periodic.el ends here
