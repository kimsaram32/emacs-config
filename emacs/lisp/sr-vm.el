;;; sr-vm.el --- Virtual machines management  -*- lexical-binding: t; -*-

;; Copyright (C) Minjeong Kim

;; Author: Minjeong Kim <kimsaram32@proton.me>
;; URL: https://github.com/kimsaram32/emacs-config

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Personal library for managing virtual machines. Uses UTM as the
;; backend. The commands are not waited on success or failure; This
;; minimizes interruptions.

;;; Code:
(defcustom sr/vm-list nil
  "List of VMs.

  Each entry is a plist with the following items:

  name                 Name of the machine.

  ssh-hostname         Hostname for SSHing into the machine.
  utm-name             Name of the machine in UTM.
  poweroff-command     Command for shutting the machine down.
  sudo-password-file   File storing the password for sudo.")
  
(defun sr/vm-completion ()
  "Complete a VM by name."
  (let ((name (completing-read
               "VM name: "
               (mapcar (lambda (vm) (plist-get vm :name)) sr/vm-list)
               nil t)))
    (seq-find (lambda (vm)
                (equal name
                       (plist-get vm :name)))
              sr/vm-list)))

(defun sr/vm-start-all ()
  "Start all VMs."
  (interactive)
  (dolist (vm sr/vm-list)
    (call-process "utmctl" nil
                  0 nil
                  "start" (plist-get vm :utm-name)))
  (message (format "Attempting to start %d VMs" (length sr/vm-list))))

(defun sr/vm-shutdown (vm)
  "Gracefully shut down VM."
  (interactive (list (sr/vm-completion)))
  (call-process "ssh" (plist-get vm :sudo-password-file)
                0 nil
                "-tt"
                "-o" "ConnectTimeout=1"
                "-o" "ConnectionAttempts=1"
                (plist-get vm :ssh-hostname)
                (plist-get vm :poweroff-command))
  (message (format "Attempting to shut down %s" (plist-get vm :name))))

(defun sr/vm-shutdown-all ()
  "Shut down all VMs."
  (interactive)
  (dolist (vm sr/vm-list)
    (sr/vm-shutdown vm))
  (message (format "Attempting to shut down %d VMs" (length sr/vm-list))))

(provide 'sr-vm)
;;; sr-vm.el ends here
