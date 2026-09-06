;;; sr-web.el --- Personal configuration for browsing the web  -*- lexical-binding: t; -*-

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

(require 'shr)
(require 'eww)
(require 'browse-url)
(require 'goto-addr)

;;; Shr

(setq shr-max-width 80)
(setq shr-use-colors nil)
(setq shr-max-image-proportion 0.7)
(setq shr-bullet "- ")

;;; EWW

(defun sr/eww-enable-diff-mode-github-diff ()
  (when (string-match-p "github\\.com.+\\.diff$" (eww-current-url))
    (diff-mode)))

(add-hook 'eww-after-render-hook #'sr/eww-enable-diff-mode-github-diff)

;;; Browse-url

;; My customization for browsing functions: (1) Use EWW by default while
;; providing an option to use external browsers. (2) Use external browsers for
;; URLs known to require JavaScript.

(setq browse-url-browser-function #'eww-browse-url)
(setq browse-url-secondary-browser-function #'browse-url-default-browser)

(setq browse-url-handlers
      '(("github\\.com.+\\.diff$" . eww-browse-url)
        ("github\\.com" . browse-url-default-browser)
        ("youtube\\.com" . browse-url-default-browser)
        ("youtu\\.be" . browse-url-default-browser)
        ("reddit\\.com" . browse-url-default-browser)
        ("lobste\\.rs" . browse-url-default-browser)
        ("localhost" . browse-url-default-browser)))

;;; Goto address mode

(keymap-set goto-address-highlight-keymap "C-c C-o" #'goto-address-at-point)

;;; Elfeed

(keymap-global-set "C-c w" 'elfeed)

(defvar sr/elfeed-to-read-tag 'to-read-list
  "Elfeed tag for 'to read' entries.")

;;;; Load entries

(defun sr/elfeed-load-entries ()
  "Load entries from elfeed.org."
  (interactive)
  (org-babel-load-file (expand-file-name "emacs/elfeed.org" sr/dotfiles-directory)))

;;;; Integration with EWW

(defun sr/elfeed-show-visit-eww ()
  "Visit the current entry with `eww'."
  (interactive)
  (when-let ((link (elfeed-entry-link elfeed-show-entry)))
    (eww link)))

;;;; Managing 'to read' entries

(defun sr/elfeed-show-add-to-read ()
  "Add current entry to 'to read' list and go to the next entry."
  (interactive)
  (when-let ((entry elfeed-show-entry))
    (elfeed-show-tag 'unread sr/elfeed-to-read-tag)
    (message "Addded entry to to-read list")
    (elfeed-show-next)))

(defun sr/elfeed-show-add-to-read ()
  "Add current entry to 'to read' list and go to the next entry."
  (interactive)
  (when-let ((entry elfeed-show-entry))
    (elfeed-show-tag 'unread sr/elfeed-to-read-tag)
    (message "Addded entry to to-read list")
    (elfeed-show-next)))

(defun sr/elfeed-search-show-to-reads ()
  "Set Elfeed filter to show 'to read' entries only."
  (interactive)
  (elfeed-search-set-filter "+to-read-list"))

(defun sr/elfeed-export-entries ()
  "Push current entries to the mark ring."
  (interactive)
  (kill-new (string-join
  	       (mapcar
  	        (lambda (entry) (elfeed-entry-link entry))
  	        (if (region-active-p)
                  (elfeed-search-selected)
                elfeed-search-entries))
  	       "\n"))
  (message "Pushed current entries to the mark ring."))

;;;; Mark entries as read

(defun sr/elfeed-search-read ()
  "Read elfeed entries in search mode."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (elfeed-untag entries 'unread)
    (elfeed-untag entries sr/elfeed-to-read-tag)
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun sr/elfeed-show-read ()
  "Read current elfeed entry."
  (interactive)
  (elfeed-show-untag 'unread 'to-read-list))

;;;; Configuration

(with-eval-after-load 'elfeed
  (setq-default elfeed-search-filter "+unread -to-read-list")
  (remove-hook 'elfeed-search-update-hook #'elfeed-search-add-separators)

  (keymap-set elfeed-show-mode-map "w" #'sr/elfeed-show-visit-eww)
  (keymap-set elfeed-show-mode-map "a" 'sr/elfeed-show-add-to-read)
  (keymap-set elfeed-show-mode-map "r" #'sr/elfeed-show-read)

  (keymap-set elfeed-search-mode-map "l" 'sr/elfeed-search-show-to-reads)
  (keymap-set elfeed-search-mode-map "e" 'sr/elfeed-export-entries)
  (keymap-set elfeed-search-mode-map "r" #'sr/elfeed-search-read)

  (sr/elfeed-load-entries))

;;; _

(provide 'sr-web)

;;; sr-web.el ends here
