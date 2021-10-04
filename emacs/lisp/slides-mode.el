;; slides mode

(define-derived-mode slides-mode latex-mode "Slides"
  "Mode for editing some kind of config files."
  (make-local-variable 'slides-indent-offset)
  (set (make-local-variable 'indent-line-function) 'slides-indent-line))

;; remove bold font for latex keywords
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; special faces
(make-face 'font-lock-special-slide-face)
(make-face 'font-lock-special-macro-face)

(set-face-foreground 'font-lock-special-slide-face "pink")
(set-face-attribute 'font-lock-special-slide-face nil :weight 'bold :underline t)

(set-face-foreground 'font-lock-special-macro-face "blue")

(defun add-custom-keyw()
  (font-lock-add-keywords nil
                          '(
                            ("\\.title\\|\\.author\\|\\.date\\|\\.institute\\|\\.meeting" . 'font-lock-special-macro-face )
                            ("\\.slidetitle\\|^\\.slide" . 'font-lock-special-slide-face )
                            ("\\.item\\|\\.iitem\\|\\.iiitem" . 'font-lock-special-macro-face )
                            ("\\.top\\|\\.bottom\\|\\.left\\|\\.right" . 'font-lock-special-macro-face )
                            ("\\.tl\\|\\.tr\\|\\.bl\\|\\.br" . 'font-lock-special-macro-face )
                            ("\\.img\\|\\.header\\|\\.scale" . 'font-lock-special-macro-face )
                            ("^#.*" . 'font-lock-comment-face )
                            )
                          )
  )

(add-hook 'slides-mode-hook 'add-custom-keyw)


(defvar slides-indent-offset 4
  "*Indentation offset for `slides-mode'.")

(defun slides-indent-line ()
  "Indent current line for `slides-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "^\.slide")
              (setq indent-col (+ indent-col slides-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "^\.slide") (>= indent-col slides-indent-offset))
        (setq indent-col (- indent-col slides-indent-offset))))
        (indent-line-to indent-col)))




(add-to-list 'auto-mode-alist '("\\.sld\\'" . slides-mode))

(provide 'slides-mode)
