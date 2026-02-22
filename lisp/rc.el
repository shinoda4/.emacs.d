;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my/new-atomic-note (title directory)
  "Create an org note named YYYYMMDDHHMM-title.org in chosen DIRECTORY."
  (interactive
   (list
    (read-string "Title: ")
    (read-directory-name "Save to directory: " "~/org/")))

  (let* ((timestamp (format-time-string "%Y%m%d%H%M"))
         (slug (replace-regexp-in-string
                "-+" "-"
                (replace-regexp-in-string
                 "[^a-z0-9]+"
                 "-"
                 (downcase title))))
         (filename (format "%s-%s.org" timestamp slug))
         (filepath (expand-file-name filename directory)))

    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert "#+title: " title "\n")
        (insert "#+created: " (format-time-string "%Y-%m-%d %H:%M") "\n")
        (insert "#+filetags: :note:\n\n")
        (insert "* " title "\n\n")))

    (find-file filepath)))

(defun my/org-export-pdf-and-open ()
  "Preview org."
  (interactive)
  (let ((file (org-latex-export-to-pdf)))
    (when file
      (org-open-file file))))

(provide 'rc)

;;; rc.el ends here.
