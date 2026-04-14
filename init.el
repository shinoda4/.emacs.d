
(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))

(set-frame-parameter nil 'fullscreen 'fullboth)

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

(use-package benchmark-init
  :straight t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package magit
  :straight t)

(use-package company
  :straight t
  :config
  (global-company-mode 1)
  (setq company-tooltip-align-annotations t)
)

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package smex
  :straight t
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ;; This is old M-x.
  ("C-c C-c M-x" . execute-extended-command))
)

(use-package ido-completing-read+
  :straight t)

(use-package python-mode
  :straight t
  :defer t
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("uv" "run" "ty" "server"))))

(file-exists-p custom-file)

(when (file-exists-p custom-file)
  (load custom-file))
