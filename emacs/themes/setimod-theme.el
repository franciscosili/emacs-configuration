;;; Adapted from seti-theme.el

(deftheme setimod
  "Seti(mod) - A theme inspired by Seti Atom Theme")

(let ((blue "#63B2FA")
      (green "#95DA96")
      (yellow "#EDDB96")
      (red "#c33932")
      (purple "#BA85CC")
      (background "#111213")
      (background-2 "#434552")
      (text "#cacecd")
      (text-highlight "#D7DCF0")
      (text-region "#56595a")
      (text-dired "#A0A0A0")
      (text-comment "#99a1a6")
      (input-text "#CCCCCC")
      (light-blue "#75E5F4")
      (dark-blue "#4f57d3")
      (intense-green "#8BE03C")
      (intense-yellow "#e3bf21")
      (text-hl "#1e2122"))

  (custom-theme-set-faces
   'setimod

   ;; Basics
   `(default ((t (:background ,background :foreground ,text))))
   `(cursor ((t (:background ,input-text :foreground ,background))))
   `(highlight ((t (:background ,text-highlight))))
   `(minibuffer-prompt ((t (:foreground ,dark-blue :weight bold))))
   `(region ((t (:background ,text-region))))
   `(error ((t (:foreground ,red :weight bold))))

   `(isearch ((t (:background ,dark-blue :foreground ,text :box (:line-width 1 :color ,dark-blue) :weight bold))))
   `(lazy-highlight ((t (:background ,background :foreground ,text :box (:line-width 1 :color ,dark-blue)))))
   `(mode-line ((t (:foreground ,text :background ,background ))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,yellow))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight ((t (:box (:line-width 3 :color ,dark-blue)))))
   `(mode-line-inactive ((t (:weight light :foreground ,text :background ,background-2))))
   `(secondary-selection ((t (:background ,background-2))))
   `(trailing-whitespace ((t (:background ,background-2))))
   `(match ((t (:weight bold :foreground ,background :background ,intense-green))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,text-comment))))
   `(font-lock-constant-face ((t (:foreground ,red))))
   `(font-lock-doc-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,green))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:weight bold :inherit (error)))))

   ;; Parens
   `(show-paren-match ((t (:foreground ,intense-yellow :background ,text-hl :weight extrabold))))
   `(show-paren-mismatch ((t (:foreground ,red :underline (:color ,red :style line)))))

   ;; Dired
   `(dired-directory ((t (:foreground ,text :weight extrabold))))
   `(dired-header ((t (:foreground ,blue  :weight bold))))
   `(dired-ignored ((t (:foreground ,text))))
   `(dired-flagged ((t (:foreground ,red :weight bold))))
   `(dired-marked ((t (:background ,blue :foreground "white" :weight normal))))
   `(dired-perm-write ((t (:foreground ,yellow :weight ultra-bold))))
   `(dired-symlink ((t (:foreground ,light-blue :weight normal))))
   `(dired-warning ((t (:inherit (font-lock-warning-face)))))

   ;; helm
   `(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground ,text))))
   `(helm-bookmark-gnus ((t (:foreground ,text))))
   `(helm-bookmark-info ((t (:foreground ,text))))
   `(helm-bookmark-man ((t (:foreground ,text))))
   `(helm-bookmark-w3m ((t (:foreground ,text))))
   `(helm-buffer-directory ((t (:foreground ,text :background ,background))))
   `(helm-buffer-file ((t (:foreground ,text :background ,background))))
   `(helm-buffer-not-saved ((t (:foreground ,text :background ,background))))
   `(helm-buffer-process ((t (:foreground ,green :background ,background))))
   `(helm-buffer-saved-out ((t (:foreground ,text :background ,background))))
   `(helm-buffer-size ((t (:foreground ,text :background ,background))))
   `(helm-candidate-number ((t (:background ,background :foreground ,green :inherit bold))))
   `(helm-ff-directory ((t (:foreground ,green :background ,background :inherit bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,green :background ,background :inherit bold))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,light-blue :background ,background :inherit bold))))
   `(helm-ff-executable ((t (:foreground ,intense-green :background ,background :weight normal))))
   `(helm-ff-file ((t (:foreground ,text :background ,background :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red :background ,background :inherit bold))))
   `(helm-ff-prefix ((t (:foreground ,background :background ,green :weight normal))))
   `(helm-ff-symlink ((t (:foreground ,light-blue :background ,background :inherit bold))))
   `(helm-grep-cmd-line ((t (:foreground ,text :background ,background))))
   `(helm-grep-file ((t (:foreground ,text :background ,background))))
   `(helm-grep-finish ((t (:foreground ,text :background ,background))))
   `(helm-grep-lineno ((t (:foreground ,text :background ,background :inherit bold))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-header ((t (:foreground ,text :background ,background :underline (:color ,dark-blue :style line)))))
   `(helm-header-line-left-margin ((t (:foreground ,green :background ,nil))))
   `(helm-match ((t (:background ,text-highlight :foreground ,text))))
   `(helm-match-item ((t (:background ,text-highlight :foreground ,intense-green))))
   `(helm-moccur-buffer ((t (:foreground ,text :background ,background))))
   `(helm-selection ((t (:background ,text-region))))
   `(helm-selection-line ((t (:background ,background))))
   `(helm-separator ((t (:foreground ,text :background ,background))))
   `(helm-source-header ((t (:foreground ,text :background ,background :underline (:color ,dark-blue :style line)))))
   `(helm-time-zone-current ((t (:foreground ,green :background ,background))))
   `(helm-time-zone-home ((t (:foreground ,text :background ,background))))
   `(helm-visible-mark ((t (:foreground ,green :background ,background))))

   ;; helm-swoop
  `(helm-swoop-target-line-block-face ((t (:foreground ,text :background ,text-highlight))))
  `(helm-swoop-target-line-face ((t (:background ,text-highlight))))
  `(helm-swoop-target-word-face ((t (:background ,text-highlight :foreground ,text))))

  `(term ((t (:foreground ,text))))
  `(term-color-black ((t (:foreground ,background))))
  `(term-color-red ((t (:foreground ,red))))
  `(term-color-green ((t (:foreground ,green))))
  `(term-color-yellow ((t (:foreground ,yellow))))
  `(term-color-blue ((t (:foreground ,blue))))
  `(term-color-magenta ((t (:foreground ,purple))))
  `(term-color-cyan ((t (:foreground ,blue))))
  `(term-color-white ((t (:foreground ,text))))

   ;; Lines
   `(linum ((t (:foreground ,text-region  :weight light :height 0.9))))
   `(fringe ((t (:background ,background :foreground ,text))))
   `(left-margin ((t (nil))))
   `(hl-line ((t (:background ,text-hl))))
   )

  (custom-theme-set-variables
   'setimod
   `(cursor-type 'bar)))

;;;###autoload
(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'setimod)
;;; setimod-theme.el ends here
