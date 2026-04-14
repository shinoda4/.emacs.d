(deftheme eidos-lumen
  "Eidos-Lumen: A high-fidelity light theme inspired by Bauhaus and Swiss typography.
   Optimized for 8+ hours of cognitive focus. Created 2026-04-15.")

(custom-theme-set-faces
 'eidos-lumen

 '(default ((t (:family "Iosevka" :width normal :height 180 :weight regular :foreground "#2d3436" :background "#f8f9fa"))))
 '(cursor ((t (:background "#0984e3"))))
 '(fringe ((t (:background "#f8f9fa"))))
 '(region ((t (:background "#dfe6e9" :extend t))))
 '(highlight ((t (:background "#f1f2f6"))))
 '(shadow ((t (:foreground "#95a5a6"))))
 '(minibuffer-prompt ((t (:foreground "#0984e3" :weight bold))))
 '(tooltip ((t (:background "#ffffff" :foreground "#2d3436" :box (:line-width 1 :color "#dfe6e9")))))

 '(font-lock-keyword-face ((t (:foreground "#6c5ce7" :weight semi-bold))))
 '(font-lock-function-name-face ((t (:foreground "#0984e3" :weight semi-bold))))
 '(font-lock-function-call-face ((t (:foreground "#0769b1"))))
 '(font-lock-string-face ((t (:foreground "#2d8a4e"))))
 '(font-lock-comment-face ((t (:foreground "#95a5a6" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#b2bec3" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#d35400" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#455a64" :weight semi-bold))))
 '(font-lock-variable-name-face ((t (:foreground "#2d3436"))))
 '(font-lock-variable-use-face ((t (:foreground "#4b4b4b"))))
 '(font-lock-number-face ((t (:foreground "#e67e22"))))
 '(font-lock-builtin-face ((t (:foreground "#d63031" :slant italic))))
 '(font-lock-operator-face ((t (:foreground "#636e72"))))
 '(font-lock-punctuation-face ((t (:foreground "#636e72"))))
 '(font-lock-bracket-face ((t (:foreground "#7f8c8d"))))
 '(font-lock-warning-face ((t (:foreground "#d63031" :weight bold :underline t))))
 '(font-lock-doc-face ((t (:foreground "#718093" :slant italic))))

 '(mode-line ((t (:background "#ffffff" :foreground "#2d3436" :box (:line-width 1 :color "#dfe6e9")))))
 '(mode-line-inactive ((t (:background "#f8f9fa" :foreground "#b2bec3" :box (:line-width 1 :color "#f1f2f6")))))
 '(mode-line-buffer-id ((t (:foreground "#0984e3" :weight bold))))

 '(isearch ((t (:foreground "#ffffff" :background "#0984e3"))))
 '(lazy-highlight ((t (:background "#e1f5fe" :foreground "#01579b"))))
 '(match ((t (:background "#fab1a0" :foreground "#2d3436"))))

 '(link ((t (:foreground "#0984e3" :underline t))))
 '(link-visited ((t (:foreground "#6c5ce7" :underline t)))))

(provide-theme 'eidos-lumen)
