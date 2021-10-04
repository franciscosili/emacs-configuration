;; -- emacs fns
;; frani

;; delete the current file and kill the buffer
(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; rename the current file/buffer
(defun rename-this-file-and-buffer (new-name)
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;; Multi-line version of just-one-space: Delete all
;; spaces, tabs and newlines around point,
;; leaving one space (or N spaces)."
(defun multi-line-just-one-space (&optional n)
  (interactive "*p")
  (let ((orig-pos (point)))
    (skip-chars-backward " \t\n")
    (constrain-to-field nil orig-pos)
    (dotimes (i (or n 1))
      (if (= (following-char) ?\s)
    (forward-char 1)
  (insert ?\s)))
    (delete-region
     (point)
     (progn
       (skip-chars-forward " \t\n")
       (constrain-to-field nil orig-pos t)))))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; move line up or down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; kill other buffers
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;; Switches between .h and .cxx files for the two cases:
;; 1) The .h and .cxx files are in the same dirctory
;; 2) path/MyPackage/MyPackage/MyClass.h
;;    path/MyPackage/src/MyClass.cxx
(defun switch-src-header ()
  (interactive)
  (setq filename buffer-file-name)
  (setq path (split-string filename "/"))
  (if (not (string-equal (car path) ""))
      (setq path (cons "" path)))
  (setq len (length path))
  (setq pkg (nth (- len 3) path))

  (if (string-match "\\.h" filename)
      (progn
        (setq hpath (replace-match ".cxx" nil nil filename))
        (if (not (file-readable-p hpath))    ; look for .cxx file in current dir first
            (progn                           ; now check in the "pkg/pkg" dir
              (setcar (nthcdr (- len 2) path) "src")
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.h" tmppath)
              (setq hpath (replace-match ".cxx" nil nil tmppath))
              )
          )
        )
    )
  (if (string-match "\\.cxx" filename)
      (progn
        (setq hpath (replace-match ".h" nil nil filename))
        (if (not (file-readable-p hpath))
            (progn
              (setcar (nthcdr (- len 2) path) pkg)
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.cxx" tmppath)
              (setq hpath (replace-match ".h" nil nil tmppath))
              )
          )
        )
    )
  (find-file hpath)
  )

;; toggle between CamelCase and snake
(defun mo-toggle-identifier-naming-style ()
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))

;; move window borders
(defun xor (b1 b2)
  "Exclusive or of its two arguments."
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
         "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
         (interactive)
         (if (null arg) (setq arg 5))
         (let ((left-edge (nth 0 (window-edges))))
           (if (xor (= left-edge 0) dir)
               (shrink-window arg t)
             (enlarge-window arg t))))

(defun move-border-left (arg)
  "If this is a window with its right edge being the edge of the screen, enlarge
     the window horizontally. If this is a window with its left edge being the edge
     of the screen, shrink the window horizontally. Otherwise, default to enlarging
     horizontally.

     Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (move-border-left-or-right arg t))

(defun move-border-right (arg)
  "If this is a window with its right edge being the edge of the screen, shrink
     the window horizontally. If this is a window with its left edge being the edge
     of the screen, enlarge the window horizontally. Otherwise, default to shrinking
     horizontally.

     Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (move-border-left-or-right arg nil))

;; swap windows
(require 'buffer-move)
(defun win-swap ()
  (interactive)
  (if (null (windmove-find-other-window 'right))
      (buf-move-left)
    (buf-move-right)
    )
  )

(defun atl-switch-src ()
  (interactive)
  (setq filename buffer-file-name)
  (setq path (split-string filename "/"))
  (if (not (string-equal (car path) ""))
      (setq path (cons "" path)))
  (setq len (length path))
  (setq pkg (nth (- len 3) path))

  (if (string-match "\\.h" filename)
      (progn
        (setq hpath (replace-match ".cxx" nil nil filename))
        (if (not(file-readable-p hpath))    ; look for .cxx file in current dir first
            (progn        ; now check inthe "pkg/pkg" dir
              (setcar (nthcdr (- len 2) path) "Root")
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.h" tmppath)
              (setq hpath (replace-match ".cxx" nil nil tmppath))
              )
          )
        (if (not(file-readable-p hpath))    ; look for .cxx file in current dir first
            (progn        ; now check inthe "pkg/pkg" dir
              (setcar (nthcdr (- len 2) path) "src")
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.h" tmppath)
              (setq hpath (replace-match ".cxx" nil nil tmppath))
              )
          )
        )
    )
  (if (string-match "\\.cxx" filename)
      (progn
        (setq hpath (replace-match ".h" nil nil filename))
        (if (not (file-readable-p hpath))
            (progn
              (setcar (nthcdr (- len 2) path) pkg)
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.cxx" tmppath)
              (setq hpath (replace-match ".h" nil nil tmppath))
              )
          )
        (if (not (file-readable-p hpath))
            (progn
              (setcar (nthcdr (- len 2) path) pkg)
              (setq tmppath (mapconcat 'identity path "/"))
              (string-match "\\.cxx" tmppath)
              (setq hpath (replace-match ".h" nil nil tmppath))
              )
          ))
    )
  (find-file hpath)
  )

;; Center text
(defun center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                      (/ (window-width) 5)
                      (/ (window-width) 5))
  )

(defun center-text-clear ()
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                      nil
                      nil))

(setq centered nil)

(defun center-text-mode ()
  (interactive)
  (if centered
      (progn (center-text-clear)
             (setq centered nil))
      (progn (center-text)
             (setq centered t))))

;; (defun execute-clatex-on-buffer ()
;;   (interactive "MRunning clatex -f")
;;   (shell-command "clatex -f" buffer-file-name)
;;   )

;; (defun clatex-on-buffer ()
;;   (shell-command (concat "clatex -f" (buffer-file-name)))
;;   )

(defun get-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789.-")
  (or (looking-at "[0123456789.-]+")
      (error "No number at point"))
  (string-to-number (match-string 0)))

(defun round-number-at-point-to-decimals (decimal-count)
  (interactive "NDecimal count: ")
  (let ((mult (expt 10 decimal-count)))
    (replace-match (number-to-string
                    (/
                     (fround
                      (*
                       mult
                       (get-number-at-point)))
                     mult)))))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))


;; Align by &
(defun align-latex-table (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun align-twiki-table (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)|" 1 1 t))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


(provide 'fns)
