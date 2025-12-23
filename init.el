
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
  (setq tab-always-indent 'complete)
  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

(use-package rust-mode
  :straight t
  )



(add-hook 'rust-mode-hook 'eglot-ensure)
