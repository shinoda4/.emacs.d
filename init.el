;;; package --- Summary ;;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(set-frame-parameter nil 'fullscreen 'fullboth)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq gc-cons-threshold (* 100 1024 1024))
(setq scroll-step 1
      scroll-consistently t
      scroll-margin 3)

(setq visible-bell t)

;; keymaps

(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-,")
                (lambda ()
                  (interactive)
                  (let ((col (current-column)))
                    (duplicate-line 1)
                    (forward-line 1)
                    (move-to-column col))))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<f9>") 'compile)

;; plugins

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package benchmark-init
  :straight t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package magit
  :defer t
  :straight t)

(use-package corfu
  :defer t
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode 1)
  (require 'corfu-info)
  ;; (setq corfu-popupinfo-delay 0.5)
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down))
  )

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package company
;;   :straight t
;;   :config
;;   (global-company-mode 1)
;;   (setq company-tooltip-align-annotations t)
;; )

;; (use-package company-box
;;   :straight t
;;   :defer t
;;   :hook (company-mode . company-box-mode))

(use-package avy
  :straight t
  :config
  ;; (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-;") 'avy-goto-char-2)
  (global-set-key (kbd "C-:") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
)

(use-package smex
  :straight t
  :defer t
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ;; This is old M-x.
  ("C-c C-c M-x" . execute-extended-command))
)

(use-package ido-completing-read+
  :straight t)

(use-package flx-ido
  :straight t
  :defer t
  :init
  (flx-ido-mode 1)
  )

;; (use-package dired+
;;   :straight t)

(use-package projectile
  :straight t
  :defer t
  :init
  (projectile-mode +1)
  :bind-keymap ("C-c C-p" . projectile-command-map))

(use-package multiple-cursors
  :straight t
  :defer t
  :init
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ))

(use-package python-mode
  :straight t
  :defer t
  )

(use-package rust-mode
  :straight t
  :defer t
  )

(use-package markdown-mode
  :straight t
  :defer t
  :init
  (setq markdown-command "pandoc")
  )

(use-package flycheck
  :straight t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'rust-clippy)
              ))
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("uv" "run" "ty" "server"))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
