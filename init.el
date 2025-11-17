
(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

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
(global-set-key (kbd "C-c C-s") 'save-buffer)
(global-set-key (kbd "C-c C-i") 'imenu)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))



(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
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


(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

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


(use-package lsp-mode
  :hook ((go-mode rust-mode) . lsp)
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-," . lsp-find-references))
  :custom
  (lsp-completion-provider :capf)
  (lsp-ui-doc-position 'at-point))

(use-package lsp-ui
  :commands lsp-ui-mode)

(global-set-key (kbd "C-c H") #'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c C-j") 'lsp-ui-imenu)
(global-set-key (kbd "C-c h") #'lsp-ui-doc-show)



(load-file "~/.emacs.d/.emacs.custom.el")
