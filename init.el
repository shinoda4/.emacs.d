
(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

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

;;; Options

;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)


(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq-default case-fold-search t)

(global-display-line-numbers-mode 1)

(add-hook 'text-mode-hook 'auto-fill-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(setq inhibit-startup-message t)

(setq tab-width 4)

(setq indent-tabs-mode nil)

;;; Keymaps

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Style

;;; Fonts

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'darwin) "Iosevka-20")))

(add-to-list 'default-frame-alist
             `(font . ,(rc/get-default-font)))

;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

;;; Hightlight the cursor line

(use-package hl-line
  :straight t
  :init
  (global-hl-line-mode))

(custom-set-faces
 '(hl-line ((t (:background "#3C3C3C")))))

;;; Packages

;; (use-package gruber-darker-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;;; Completion

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-auto-trigger ".")	   ;; Custom trigger characters
  (corfu-quit-no-match 'separator) ;; or t
  
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

(use-package dabbrev
  :straight t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(setq global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p mct--active)
                 (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))

;; (use-package puni
;;   :ensure t
;;   :hook (prog-mode . puni-mode)
;; )

;;; Language Support Protocol

(use-package lsp-mode
  :straight t
  :init
  :commands lsp
  :hook ((go-ts-mode rust-ts-mode yaml-ts-mode python-ts-mode typescript-ts-mode tsx-ts-mode tex-ts-mode) . lsp)
  :bind (:map lsp-mode-map
	      ("M-." . lsp-find-definition)
	      ("M-," . lsp-find-references))
  :config
  ;;  (setq lsp-log-io t)
  (setq lsp-enable-indentation nil)

  ;; :custom
  ;; (lsp-idle-delay 0.500)
  ;; (lsp-completion-provider :none)
  ;; (lsp-headerline-breadcrumb-enable nil))
  )

(use-package rust-mode
  :straight t
  :init
  ;;  (setq rust-mode-treesitter-derive t)
  )

(use-package typescript-mode
  :straight t
  :config
  )

(use-package auctex
  :straight t
  )
(setq preview-inhibit t)
(setq LaTeX-preview-setup nil)
(setq TeX-engine 'xetex)
(setq TeX-command-default "XeLaTeX")


(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq typescript-ts-mode-indent-offset 4
                  indent-tabs-mode nil)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq tsx-ts-mode-indent-offset 4
                  indent-tabs-mode nil)))


(use-package yaml-mode
  :straight t
  )

(use-package python-mode
  :straight t
  )

(use-package tex-mode
  :straight t
  )



;;; Formatting

(straight-use-package 'apheleia)

(apheleia-global-mode +1)

(setq apheleia-formatters
      '((prettier . ("prettier" "--stdin-filepath" filepath))
        (black . ("black" "-"))))

(setq apheleia-mode-alist
      '((typescript-mode . prettier)
        (python-mode . black)))


;;; Flycheck

(use-package flycheck
  :straight t
   :init (global-flycheck-mode))

;;; Treesitter

(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)
	(typescript-mode . typescript-ts-mode)
	))
;;	(tsx-mode . tsx-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
	(go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
	(python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")))

(setq treesit-extra-load-path (list "~/.emacs.d/tree-sitter"))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;;; Themes

(use-package modus-themes
  :straight t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; Your customizations here:
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
	modus-themes-to-rotate modus-themes-items
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui t
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-completions '((t . (bold)))
	modus-themes-prompts '(bold)
	modus-themes-headings
	'((agenda-structure . (variable-pitch light 2.2))
	  (agenda-date . (variable-pitch regular 1.3))
	  (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-vivendi))
