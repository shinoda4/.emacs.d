;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'rc)

;; straight.el
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

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

;; editor configurations
(require 'org-tempo)
(setq org-use-speed-commands t)

(recentf-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(if (display-graphic-p)
    (scroll-bar-mode 0)
    )
(electric-pair-mode 1)
(electric-quote-mode 1)
(global-display-line-numbers-mode 1)

;; keymaps
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

;; third part plugins
(use-package ido-completing-read+
  :straight t
  :config
  (require 'ido-completing-read+)
  )

;; (ido-mode 1)
;; (ido-everywhere 1)
;; (ido-ubiquitous-mode 1)

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-20"))

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t)
  )

(use-package magit
  :straight t)

;; (use-package smex
;;   :straight t)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  ;; (completion-pcm-leading-wildcard t)
  )

;; VERTical Interactive COmpletion
(use-package vertico
  :straight t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package consult
  :straight t
  :config
  (setq consult-preview-key '(:debounce 0.5 any))
  :bind
  (("C-c f" . 'consult-fd)
   ("C-c r" . 'consult-recent-file))
  )

(use-package paredit
  :straight t
  )

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; for buffer completition
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)

(use-package multiple-cursors
  :straight t
  :bind (
    ("C-S-c C-S-c" . mc/edit-lines)
    ("C->"         . mc/mark-next-like-this)
    ("C-<"         . mc/mark-previous-like-this)
    ("C-c C-<"     . mc/mark-all-like-this)
    ))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package flycheck
  :straight t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'rust-ts-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'rust-clippy)
              ))
  )

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(use-package impatient-mode
  :straight t)

;; treesitter language source list
(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
        (go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir.git" "v0.3.4" "src")
        (typst "https://github.com/uben0/tree-sitter-typst")
        ))

(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)
        (python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        ))

;; eglot hooks
(use-package rust-mode
  :straight t)

(require 'eglot)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Mono" :height 160)

(load custom-file 'noerror)
;;; init.el ends here
