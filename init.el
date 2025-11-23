
(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))


(defun rc/get-default-font ()
  (cond
   ((eq system-type 'darwin) "Iosevka-20")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(setq compilation-scroll-output 'first-error)

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(add-to-list 'default-frame-alist '(fullscreen . fullboth))


(setq display-line-numbers-width 4)
(setq display-line-numbers-type 'relative) ;; 或 'visual 或 'relative
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

(setq inhibit-startup-message t)
(set-face-attribute 'default nil :height 200)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-c C-r") 'recentf-open-files))

(global-set-key (kbd "C-c C-l") 'load-file)
(global-set-key (kbd "C-c C-g") 'goto-line)
;; (global-set-key (kbd "C-c C-s") 'save-buffer)
(global-set-key (kbd "C-c C-i") 'imenu)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-t") 'term)
(global-set-key (kbd "C-c C-q") 'ibuffer)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ibuffer

  :bind ("C-x C-b" . ibuffer))

(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs"
          (name . "^\\*"))
         ("Code"
          (mode . prog-mode))
         ("Dired"
          (mode . dired-mode))
         ("Term"
          (mode . term-mode)))))


(use-package projectile
  :ensure t
  :init
  (projectile-mode +1) 
  :config
    (setq projectile-project-root-files-top-down-recurring
        '(".git"))
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "M-p f") 'projectile-find-file)
  )

(defun my/set-default-directory-to-project-root ()
  "Set `default-directory` to Projectile project root if inside a project."
  (let ((proj (projectile-project-root)))
    (when proj
      (setq default-directory proj))))

(add-hook 'find-file-hook 'my/set-default-directory-to-project-root)

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))



(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-c k" . counsel-rg)))


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))


(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1))

(global-set-key (kbd "C-c C-F") 'counsel-git)

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package lsp-mode
  :hook ((go-ts-mode rust-ts-mode) . lsp)
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-," . lsp-find-references))
  :config
  (setq lsp-ui-imenu-window-width 0)
  (setq truncate-lines t)
  (global-set-key (kbd "C-c H") #'lsp-describe-thing-at-point)
  (global-set-key (kbd "C-c C-j") 'lsp-ui-imenu)
  (global-set-key (kbd "C-c h") #'lsp-ui-doc-show)
  (global-set-key (kbd "C-c C-p") #'lsp-ui-find-workspace-symbol)

  :custom
  (lsp-completion-provider :capf)
  (lsp-ui-doc-position 'at-point)
  (lsp-enable-completion-at-point t))

(add-hook 'lsp-ui-imenu-mode-hook
          (lambda ()
            (setq-local truncate-lines t)
            (redraw-display)))  ;; 强制刷新显示


(setq lsp-rust-analyzer-cargo-watch-command "clippy")



(setq lsp-log-io t)

(use-package lsp-ui
  :commands lsp-ui-mode)


(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)
        (go-mode . go-ts-mode)))


(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
	(go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))


(load-file "~/.emacs.d/.emacs.custom.el")
