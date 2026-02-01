
(require 'org-tempo)

;; Some options

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(add-to-list 'exec-path "/Users/carl/.local/bin")
(setenv "PATH" (concat "/Users/carl/.local/bin:" (getenv "PATH")))

(setq custom-file
      (if (getenv "XDG_CONFIG_HOME")
	  (expand-file-name "emacs/.emacs.custom.el" (getenv "XDG_CONFIG_HOME"))
	(cond
	 ((eq system-type 'darwin)
	  (expand-file-name "~/.config/emacs/.emacs.custom.el")
	  )
	 (t (expand-file-name ".emacs.custom.el" user-emacs-directory))
	 )))

(load custom-file 'noerror)
(recentf-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(winner-mode 1)
(electric-pair-mode 1)

;; (global-tab-line-mode 1) ;; Enable tab bar
;; (setq-default visual-line-mode t)
(setq-default truncate-lines t)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq confirm-kill-emacs 'yes-or-no-p)
(setq-default case-fold-search t)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq inhibit-startup-message t)

(setq org-hide-emphasis-markers t)

(delete-selection-mode 1)
(setq skeleton-pair t)
(global-set-key (kbd "~") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "*") 'skeleton-pair-insert-maybe)

;; (add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda ()
  (setq truncate-lines t)))

;; fonts

;; (defun get-default-font()
;;   (cond
;;    ((eq system-type 'darwin)
;;     "TX-02")
;;    ))

;; (add-to-list 'default-frame-alist
;; 	     `(font . ,(get-default-font)))

(set-fontset-font t 'han (font-spec :family "Kaiti SC"))


(set-face-attribute 'default nil 
                    :family "TX-02" 
                    :height 120 
                    :width 'ultra-condensed) ; ultra-condensed, condensed, normal, expanded


(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
        (go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")))


;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
(setq mac-pass-command-to-system nil)

;; (setq eww-search-prefix "https://www.google.com/search?gbv=1&q=")

(global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c f") 'consult-fd)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(provide 'basic)


