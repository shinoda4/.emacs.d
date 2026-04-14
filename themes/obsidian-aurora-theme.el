(deftheme obsidian-aurora
  "Obsidian Aurora — A dark theme forged from deep space and northern lights.
   Palette inspired by the harmony of cool deep blacks, aurora greens,
   violet nebulae, and warm amber accents. Created 2026-04-15.")

;; ============================================================
;; COLOR PALETTE REFERENCE
;; ============================================================
;; Background Scale (cool-tinted blacks, NOT pure #000):
;;   bg-deep    #080b12  — deepest background, outer chrome
;;   bg-base    #0e1117  — main editor background
;;   bg-surface #141922  — panels, sidebars
;;   bg-raised  #1b2130  — floating UI, tooltips
;;   bg-overlay #222a3a  — selection, highlights
;;   bg-select  #2a3448  — active selection
;;
;; Foreground Scale (cool-white with slight blue tint):
;;   fg-dim     #4a5568  — comments, muted
;;   fg-muted   #6b7a99  — shadow, inactive
;;   fg-subtle  #8896b3  — secondary text
;;   fg-base    #c9d1e0  — primary text (NOT pure white — reduces strain)
;;   fg-bright  #e8edf5  — emphasis text
;;
;; Aurora Accent Palette (jewel-tones, perceptually balanced):
;;   aurora-green   #3ddba4  — strings, success   (cool mint)
;;   aurora-teal    #2ec4e8  — builtins, links     (sky cyan)
;;   aurora-blue    #6699ff  — functions           (electric indigo-blue)
;;   aurora-violet  #b48eff  — types, macros       (soft amethyst)
;;   aurora-pink    #ff6eb4  — keywords            (hot magenta)
;;   aurora-red     #ff5f6e  — errors, warnings    (coral red)
;;   aurora-orange  #ffaa55  — numbers, escapes    (warm amber)
;;   aurora-yellow  #ffe066  — constants, cursor   (golden)
;;
;; Semantic roles:
;;   keyword      aurora-pink    #ff6eb4
;;   function     aurora-blue    #6699ff
;;   type         aurora-violet  #b48eff
;;   string       aurora-green   #3ddba4
;;   constant     aurora-yellow  #ffe066
;;   number       aurora-orange  #ffaa55
;;   builtin      aurora-teal    #2ec4e8
;;   comment      fg-dim         #4a5568
;;   variable     fg-subtle      #8896b3
;;   property     fg-muted       #6b7a99
;;   operator     aurora-violet  #9980cc  (desaturated)
;;   punctuation  #3d4a60
;; ============================================================

(custom-theme-set-faces
 'obsidian-aurora

 ;; ── Core / Global ─────────────────────────────────────────
 '(default ((t (:family "Iosevka"
                :foundry "nil"
                :width normal
                :height 180
                :weight regular
                :slant normal
                :underline nil
                :overline nil
                :extend nil
                :strike-through nil
                :box nil
                :inverse-video nil
                :foreground "#c9d1e0"
                :background "#0e1117"
                :stipple nil
                :inherit nil))))

 ;; Cursor: golden yellow — maximum visibility, warm contrast
 '(cursor ((t (:background "#ffe066"))))

 '(fixed-pitch     ((t (:family "Monospace"))))
 '(variable-pitch  ((t (:family "Sans Serif"))))

 ;; ── Special glyphs ────────────────────────────────────────
 ;; escape-glyph: sky cyan — stands out without alarming
 '(escape-glyph ((t (:foreground "#2ec4e8"))))
 ;; homoglyph: warm amber — gentle warning
 '(homoglyph ((t (:foreground "#ffaa55"))))

 ;; ── Minibuffer ────────────────────────────────────────────
 '(minibuffer-prompt ((t (:foreground "#ffe066" :weight semi-bold))))

 ;; ── Selection & Highlights ────────────────────────────────
 ;; highlight: subtle cool blue-grey tint, not distracting
 '(highlight           ((t (:background "#1b2130"))))
 '(region              ((t (:extend t :background "#2a3448"))))
 '(secondary-selection ((t (:extend t :background "#222a3a"))))
 '(shadow              ((t (:foreground "#4a5568"))))
 '(trailing-whitespace ((t (:background "#3d1525"))))

 ;; ── Font Lock — Syntax Highlighting ───────────────────────
 ;; Brackets: dim punctuation — don't compete with code
 '(font-lock-bracket-face
   ((t (:inherit (font-lock-punctuation-face) :foreground "#3d4a60"))))

 ;; Builtins: aurora teal — distinctive, cool, technical feel
 '(font-lock-builtin-face
   ((t (:foreground "#2ec4e8" :slant italic))))

 ;; Comments: steel-blue muted — clearly secondary, readable
 '(font-lock-comment-delimiter-face
   ((t (:foreground "#3d4a60" :slant italic))))
 '(font-lock-comment-face
   ((t (:foreground "#4a5568" :slant italic))))

 ;; Constants: golden yellow — high importance, warm standout
 '(font-lock-constant-face
   ((t (:foreground "#ffe066"))))

 '(font-lock-delimiter-face
   ((t (:inherit (font-lock-punctuation-face)))))

 ;; Doc strings: desaturated mint — readable, calmer than strings
 '(font-lock-doc-face
   ((t (:foreground "#2a9e78" :slant italic))))

 ;; Doc markup (e.g. @param): soft violet — tagged, structured
 '(font-lock-doc-markup-face
   ((t (:foreground "#9980cc" :slant italic))))

 ;; Escape sequences: warm amber + bold — must not be missed
 '(font-lock-escape-face
   ((t (:foreground "#ffaa55" :weight bold))))

 ;; Function calls: electric indigo-blue — primary action color
 '(font-lock-function-call-face
   ((t (:foreground "#5588ee"))))
 '(font-lock-function-name-face
   ((t (:foreground "#6699ff"))))

 ;; Keywords: hot magenta — most distinctive, language structure
 '(font-lock-keyword-face
   ((t (:foreground "#ff6eb4"))))

 ;; Negation: coral red — semantically "stop / negate"
 '(font-lock-negation-char-face
   ((t (:foreground "#ff5f6e"))))

 ;; Numbers: warm amber — numerics feel tangible, warm
 '(font-lock-number-face
   ((t (:foreground "#ffaa55"))))

 '(font-lock-misc-punctuation-face
   ((t (:inherit (font-lock-punctuation-face) :foreground "#4a5a72"))))

 ;; Operators: desaturated violet — present but not loud
 '(font-lock-operator-face
   ((t (:foreground "#9980cc"))))

 ;; Preprocessor: bright violet — macro-level, above code
 '(font-lock-preprocessor-face
   ((t (:foreground "#b48eff"))))

 ;; Properties: cool grey-blue italic — metadata feel
 '(font-lock-property-name-face
   ((t (:foreground "#8896b3" :slant italic))))
 '(font-lock-property-use-face
   ((t (:foreground "#6b7a99"))))

 ;; Punctuation: deep cool blue — structural but subordinate
 '(font-lock-punctuation-face
   ((t (:foreground "#3d4a60"))))

 ;; Regexp: amber backslash + violet construct — clear distinction
 '(font-lock-regexp-grouping-backslash
   ((t (:foreground "#ffaa55" :weight bold))))
 '(font-lock-regexp-grouping-construct
   ((t (:foreground "#b48eff" :weight bold))))

 ;; Strings: aurora mint green — classic, fresh, readable
 '(font-lock-string-face
   ((t (:foreground "#3ddba4"))))

 ;; Types: amethyst violet — elevated, structural, distinct from functions
 '(font-lock-type-face
   ((t (:foreground "#b48eff"))))

 ;; Variables: cool grey-blue — present, neutral, not competing
 '(font-lock-variable-name-face
   ((t (:foreground "#8896b3"))))
 '(font-lock-variable-use-face
   ((t (:foreground "#7080a0"))))

 ;; Warnings: coral red bold — cannot be ignored
 '(font-lock-warning-face
   ((t (:foreground "#ff5f6e" :weight bold))))

 ;; ── Links ─────────────────────────────────────────────────
 '(button       ((t (:inherit (link)))))
 '(link         ((t (:underline (:style line) :foreground "#2ec4e8"))))
 '(link-visited ((t (:underline (:style line) :foreground "#b48eff"))))

 ;; ── Chrome / UI ───────────────────────────────────────────
 '(fringe ((t (:background "#0e1117"))))

 ;; Header line: subtle, distinguished from mode-line
 '(header-line
   ((t (:box nil
        :foreground "#6b7a99"
        :background "#141922"
        :inherit (mode-line)))))

 ;; Tooltip: slightly raised surface
 '(tooltip
   ((t (:foreground "#c9d1e0"
        :background "#1b2130"
        :inherit (variable-pitch)))))

 ;; Mode line: thin border, muted — active
 '(mode-line
   ((t (:box (:line-width (1 . -1) :color "#2a3448" :style released-button)
        :foreground "#8896b3"
        :background "#141922"))))

 ;; Mode line buffer name: golden, always findable
 '(mode-line-buffer-id
   ((t (:foreground "#ffe066" :weight bold))))

 '(mode-line-emphasis
   ((t (:weight bold :foreground "#e8edf5"))))

 ;; Mode line highlight: cyan border on hover/focus
 '(mode-line-highlight
   ((t (:box (:line-width (2 . 2) :color "#2ec4e8" :style released-button)))))

 ;; Inactive mode line: nearly invisible, recedes
 '(mode-line-inactive
   ((t (:weight light
        :box (:line-width (1 . -1) :color "#141922" :style nil)
        :foreground "#3d4a60"
        :background "#0a0d14"
        :inherit (mode-line)))))

 ;; ── Search ────────────────────────────────────────────────
 ;; isearch: violet on deep purple — jewel-like, unmistakable
 '(isearch
   ((t (:foreground "#e8edf5" :background "#5533aa"))))

 ;; isearch fail: red on dark maroon — failure state
 '(isearch-fail
   ((t (:foreground "#ff5f6e" :background "#2a0e12"))))

 ;; lazy-highlight: dim violet — nearby matches, not current
 '(lazy-highlight
   ((t (:distant-foreground "#c9d1e0" :background "#1e1540"))))

 ;; match: deep indigo — generic match highlight
 '(match
   ((t (:background "#25204a"))))

 '(next-error    ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'obsidian-aurora)
