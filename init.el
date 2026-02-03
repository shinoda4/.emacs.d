
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'basic)

;; User defined keymaps should after C-x r <KEY>.

;; Define a keybinding for hello world.

(defun greet()
  "Hello World!"
  (interactive)
  (insert "Hello World!"))

(global-set-key (kbd "C-c h") 'greet)

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

;; (use-package gruber-darker-theme
;;   :straight t
;;   :config
;;   (load-theme 'gruber-darker t)
;;   )

;; (use-package doom-themes
;;   :straight t
;;   :custom
;;   ;; Global settings (defaults)
;;   (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
;;   (doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; for treemacs users
;;   (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   :config
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (nerd-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))


(use-package magit
  :straight t
  )

(straight-use-package 'rust-mode)

;; support vertical interface of completion or ...
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
	("DEL" . vertico-directory-delete-char)))

;; live preview
(use-package consult
  :straight t
  :bind
  (("C-x f" . consult-find)
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
  ;; (corfu-auto nil)
  
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)               
  :init
  (global-corfu-mode)
  :bind
  (:map global-map
        ("M-i" . completion-at-point))
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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("ty" "server"))))

(add-hook 'python-mode-hook 'eglot-ensure)


(use-package projectile
  :straight t
  :init
  (projectile-mode +1))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; (use-package smartparens
;;   :straight t
;;   :hook (prog-mode org-mode)
;;   :config
;;   (require 'smartparens-config))

(use-package impatient-mode
  :straight t)

(use-package multiple-cursors
  :straight t
  :bind (
    ("C-S-c C-S-c" . mc/edit-lines)          ; 选中区域后，在每一行末尾加光标
    ("C->"         . mc/mark-next-like-this) ; 选中一个词，按一下多选一个相同的
    ("C-<"         . mc/mark-previous-like-this)
    ("C-c C-<"     . mc/mark-all-like-this)  ; 一次性选中全文所有相同的词
    ))

(use-package gruvbox-theme
  :straight t
)
(use-package lorem-ipsum
  :straight t
  :init
  (lorem-ipsum-use-default-bindings)
  )

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode lisp-mode) . enable-paredit-mode)
  :config
  (bind-key "RET" #'newline-and-indent paredit-mode-map)
  )
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(elixir-mode "/usr/local/elixir-ls/language_server.sh"))
  )


(use-package yasnippet
  :straight t
  :diminish yas-minor-mode 
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-reload-all)       
  (yas-global-mode 1))

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))
