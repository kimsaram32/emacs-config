;; -*- lexical-binding: t -*-

(defun me:ensure-lexical-binding ()
  "Ensure lexical binding of the current buffer."
  (message "HI")
  (goto-char (point-min))
  (insert ";; -*- lexical-binding: t -*-")
  (newline)
  (newline)
  (basic-save-buffer)
  (kill-buffer))

(setq org-use-extra-keys t) ;; this should be enabled before loading org

(require 'org)
(add-hook 'org-babel-post-tangle-hook #'me:ensure-lexical-binding)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))

