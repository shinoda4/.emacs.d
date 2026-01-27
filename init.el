
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


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

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

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind (
	 :map vertico-map
         ("DEL" . vertico-directory-delete-char)))

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
