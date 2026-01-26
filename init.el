
(setq custom-file "~/.emacs.d/.emacs.custom.el")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; (setq initial-frame-alist '((maximized . t))) 
(setq initial-frame-alist '((fullscreen . fullboth)))

(recentf-mode 1)

(define-key global-map (kbd "C-c C-r") 'recentf-open-files)

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

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t)
  )

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'darwin) "Iosevka-20")))

(add-to-list 'default-frame-alist
	     `(font . ,(rc/get-default-font)))


(use-package corfu
  :straight t
  ;; Optional customizations
  
  :custom
  (corfu-auto t)                 
  (corfu-auto-prefix 2)          
  (corfu-auto-delay 0.1)         
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  ;; (setq tab-always-indent 'complete)
  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )


(require 'eglot)

(use-package rust-mode
  :straight t
  )

(add-hook 'rust-mode-hook 'eglot-ensure)

(use-package elixir-mode
  :straight t
  )

(add-hook 'elixir-mode-hook 'eglot-ensure)


(add-to-list 'eglot-server-programs '(elixir-mode "/usr/local/elixir-ls/language_server.sh"))

(use-package orderless
  :straight t
  
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles orderless partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))


(use-package consult
  :straight t
  :bind (;; 替代 switch-to-buffer (C-x b)
         ("C-x b" . consult-buffer)
         ;; 替代 swiper (C-s)
         ("C-s" . consult-line)
         ;; 替代 counsel-imenu (跳转标题)
         ("M-g i" . consult-imenu)
         ;; 替代 counsel-recentf (你在上个问题里想要的)
         ("C-c C-r" . consult-recent-file)
         ;; 更好的 Yank (粘贴) 历史
         ("M-y" . consult-yank-pop)))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
(put 'downcase-region 'disabled nil)

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust.git" "v0.21.2")
	(go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "typescript/src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript.git" "v0.23.2" "tsx/src")
	(python "https://github.com/tree-sitter/tree-sitter-python.git" "v0.23.6" "src")))

(use-package gptel
  :straight t
  )


;; OPTIONAL configuration
(setq
 gptel-model 'mistral:latest
 gptel-backend (gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(qwen2.5-coder:1.5b)))          ;List of models

(defun greet ()
  "Greet the world."
  (interactive)
  (insert "Hello, World!"))

(global-set-key (kbd "C-c h") 'greet)




