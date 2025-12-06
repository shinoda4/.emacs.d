

(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))


;;; Fonts

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'darwin) "Iosevka-20")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;;; Options

(setq-default case-fold-search t)

(global-display-line-numbers-mode 1)

(add-hook 'text-mode-hook 'auto-fill-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)


;;; Hightlight the cursor line

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode))

(custom-set-faces
  '(hl-line ((t (:background "#3C3C3C"))))) 
  
;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; (use-package gruber-darker-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))

;;; Completion

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-auto-trigger ".") ;; Custom trigger characters
  (corfu-quit-no-match 'separator) ;; or t
  
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

(use-package dabbrev
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

;;; Language Support Protocol

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode . ("typescript-language-server", "--stdio"))))

(use-package rust-mode
  :init
  ;;  (setq rust-mode-treesitter-derive t)
  )

(use-package typescript-mode
  :ensure t)

(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;;; Treesitter


(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)
	(typescript-mode . typescript-ts-mode)))
;;	(tsx-mode . tsx-ts-mode)))


(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
	(go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")))

(setq treesit-extra-load-path (list "~/.emacs.d/tree-sitter"))

;;; Themes

(use-package modus-themes
  :ensure t
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
