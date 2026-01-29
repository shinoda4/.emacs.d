

;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
(setq mac-pass-command-to-system nil)

(global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c f") 'consult-fd)

(provide 'keymaps)
