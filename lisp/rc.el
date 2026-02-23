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
         (filepath (expand-file-name filename directory))
         (dir (file-name-directory filepath)))

    (unless (file-directory-p dir)
      (make-directory dir t))

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

(defun my/download-convert-and-clean-webp (url filename)
  "Download a image using wget, convert it to JPG via ImageMagick, and delete the original."
  (interactive "sEnter URL: \nsEnter base filename (e.g., emiri): ")
  (let* ((download-file filename)
         (jpg-file (concat filename ".jpg"))
         ;; Construct the shell command chain: Download -> Convert -> Remove
         ;; Use && to ensure the next command only runs if the previous one succeeded
         (command (format "wget -O %s %s && magick %s %s && rm %s"
                          (shell-quote-argument download-file)
                          (shell-quote-argument url)
                          (shell-quote-argument download-file)
                          (shell-quote-argument jpg-file)
                          (shell-quote-argument download-file))))
    (message "Processing image (Download -> Convert -> Clean)...")
    ;; Execute asynchronously to keep Emacs responsive during download
    (async-shell-command command "*Image Process*")
    ;; Insert the Org-mode link for the resulting JPG
    (insert (format "[[file:./%s]]" jpg-file))))

(defun my/org-file-drop-extended (orig uri action)
  (let* ((file (dnd-get-local-file-name uri t))
         (extra "Insert & Copy to ./images")

         (choice
          (completing-read
           "What to do with file? "
           (append
            '("Attach" "Open" "Insert File Link")
            (list extra)))))

    (cond
     ((string= choice extra)
      (my/org-dnd-insert-image file))

     (t
      (funcall orig uri action)))))

;; (with-eval-after-load 'org
;;   (advice-add 'org-file-drop :around #'my/org-file-drop-extended))


(defun my/org-dnd-insert-image (file)
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))

    (let* ((ext (downcase (file-name-extension file)))
           (img-ext '("png" "jpg" "jpeg" "gif" "webp" "svg")))

      (when (member ext img-ext)

        (let* ((org-dir (file-name-directory (buffer-file-name)))
               (img-dir (expand-file-name "images" org-dir))
               (timestamp (format-time-string "%Y%m%d-%H%M%S"))
               (base (file-name-base (buffer-file-name)))
               (new-name (format "%s-%s.%s" timestamp base ext))
               (target (expand-file-name new-name img-dir)))

          (unless (file-directory-p img-dir)
            (make-directory img-dir t))

          (copy-file file target t)

          (insert (format "[[file:%s]]"
                          (file-relative-name target org-dir)))

          (org-display-inline-images)

          t)))))

(provide 'rc)

;;; rc.el ends here.
