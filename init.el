
;; User defined keymaps should after C-x r <KEY>.

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
(setq dired-kill-when-opening-new-dired-buffer t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq confirm-kill-emacs 'yes-or-no-p)
(setq-default case-fold-search t)

(setq tab-width 4)
(setq inhibit-startup-message t)

;; fonts

(defun get-default-font()
  (cond
   ((eq system-type 'darwin)
    "Iosevka-20")
   ))

(add-to-list 'default-frame-alist
	     `(font . ,(get-default-font)))

;; Define a keybinding for hello world.

(defun greet()
  "Hello World!"
  (interactive)
  (insert "Hello World!"))

(global-set-key (kbd "C-c h") 'greet)


(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
        (go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")))


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

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t)
  )

(straight-use-package 'rust-mode)

;; support vertical interface of completion or ...
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind (
	 :map vertico-map
         ("DEL" . vertico-directory-delete-char)))

;; live preview
(use-package consult
  :straight t
  :bind (
         ("C-x f" . consult-find)
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-g i" . consult-imenu)
         ("C-x r r" . consult-recent-file)
         ("M-y" . consult-yank-pop)))

;; using space as separator
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))
;;  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; shows metadata on the sider
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; for completion
(use-package corfu
  :straight t

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode)
  
  ;; (setq tab-always-indent 'complete)
  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; put file path into corfu for completion
(use-package cape
  :straight t)

(add-to-list 'completion-at-point-functions #'cape-file)

(use-package vterm
  :straight t)

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(with-eval-after-loxXad 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("ty" "server"))))

(add-hook 'python-mode-hook 'eglot-ensure)



