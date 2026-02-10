;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'org-tempo)

;; org mode directory
;; (setq org-agenda-files (list (expand-file-name "~/org_mode/")))
(setq org-agenda-files (directory-files-recursively "~/org_mode/" "\\.org$"))

(setq org-use-speed-commands t)

(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)
        (python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        ))

;; Some options

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; (add-to-list 'exec-path "/Users/carl/.local/bin")
;; (setenv "PATH" (concat "/Users/carl/.local/bin:" (getenv "PATH")))

(setq custom-file
      (if (getenv "XDG_CONFIG_HOME")
	  (expand-file-name "emacs/.emacs.custom.el" (getenv "XDG_CONFIG_HOME"))
	(cond
	 ((eq system-type 'darwin)
	  (expand-file-name "~/.config/emacs/.emacs.custom.el")
	  )
	 (t (expand-file-name ".emacs.custom.el" user-emacs-directory))
	 )))

(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))

  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))

  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))


(column-number-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
;; (global-set-key (kbd "*") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "~") 'skeleton-pair-insert-maybe)
(load custom-file 'noerror)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(recentf-mode 1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(global-subword-mode 1)
(setq auto-revert-verbose nil)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq global-auto-revert-non-file-buffers t)
(setq inhibit-startup-message t)
;; (setq org-hide-emphasis-markers t)
(setq skeleton-pair t)
(setq tab-width 4)
(setq-default case-fold-search t)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines nil)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(winner-mode 1)
;; (global-tab-line-mode 1) ;; Enable tab bar
;; (setq-default visual-line-mode t)

;; (add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda ()
  (setq truncate-lines t)))


(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
        (go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir.git" "v0.3.4" "src")
        (typst "https://github.com/uben0/tree-sitter-typst")
        ))


;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
(setq mac-pass-command-to-system nil)

;; (setq eww-search-prefix "https://www.google.com/search?gbv=1&q=")

(global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)


;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)

;; fonts

;; (defun get-default-font()
;;   (cond
;;    ((eq system-type 'darwin)
;;     "TX-02")
;;    ))

;; (add-to-list 'default-frame-alist
;; 	     `(font . ,(get-default-font)))

(if (fboundp 'set-fontset-font) (set-fontset-font t 'han (font-spec :family "Kaiti SC")))

(add-to-list 'face-font-rescale-alist '("Kaiti SC" . 0.9))

(setq default-frame-alist (assoc-delete-all 'font default-frame-alist))

(let ((my-font "Fira Code-18"))
  (set-face-attribute 'default nil :font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font)))

;; (let ((my-font "Iosevka Nerd Font Mono-18"))
;;   (set-face-attribute 'default nil :font my-font :width 'ultra-condensed)
;;   (add-to-list 'default-frame-alist `(font . ,my-font)))

(set-face-attribute 'variable-pitch nil :family "IBM Plex Mono" :height 160)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(load-theme 'modus-operandi-deuteranopia)

(provide 'basic)

;;; basic.el ends here


