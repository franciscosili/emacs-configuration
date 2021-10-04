;; -- emacs init --
;; -- fran sili

;; default window size when displaying graphics
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              ;;(tool-bar-lines . 0)
              (width . 197) ; chars
              (height . 70) ; lines
              ;;(background-color . "honeydew")
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              ;;(tool-bar-lines . 0)
              (width . 220)
              (height . 100)
              ;;(background-color . "honeydew")
              (left . 50)
              (top . 50))))
  ;;(progn
  ;;  (setq initial-frame-alist '( (tool-bar-lines . 0)))
  ;;  (setq default-frame-alist '( (tool-bar-lines . 0))))
)
(if (not (display-graphic-p))
    (add-to-list 'default-frame-alist '(background-color . nil)))


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; packages
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (package-initialize)
;;   )

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;; custom file
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; setup load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no splash screen please
(setq inhibit-startup-message t)

;; enter syntaxis color mode
(global-font-lock-mode t)

;; highlight current line
(global-hl-line-mode 1)

;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; show matching paretheses
(require 'paren)
(show-paren-mode)
(setq show-paren-mismatch t)

;; tab == 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
     `((".*" . , "~/tmp/emacs-backups/")))
(setq auto-save-file-name-transforms
     `((".*" , "~/tmp/emacs-backups/" t)))

;; don't add blank lines at the end
(setq next-line-add-newlines nil)

;; scroll line by line
(setq scroll-step 1)

;; window title
(setq frame-title-format "emacs : %b")
(setq icon-title-format "emacs : %b")

;; save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; switch windows with shift + arrows
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; setup extensions
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'setup-yasnippet)

;; sane defaults
(require 'sane-defaults)

;; remove trailing spaces
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; autoloads (aka, the way to make emacs fast)
(autoload 'hide-ifdef-define "hideif" nil t)
(autoload 'hide-ifdef-undef  "hideif" nil t)
(autoload 'c-mode "cc-mode" "C Editing Mode" t)
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'insert-c++-seperator-line "e-seperators" nil t)
(autoload 'insert-c-seperator-line "e-seperators" nil t)
(autoload 'insert-elisp-seperator-line "e-seperators" nil t)
(autoload 'insert-script-seperator-line "e-seperators" nil t)
(autoload 'insert-c-section-header "e-seperators" nil t)
(autoload 'insert-c++-section-header "e-seperators" nil t)
(autoload 'insert-elisp-section-header "e-seperators" nil t)
(autoload 'insert-script-section-header "e-seperators" nil t)
(autoload 'insert-c++-big-header "e-seperators" nil t)
(autoload 'insert-elisp-big-header "e-seperators" nil t)
(autoload 'insert-script-big-header "e-seperators" nil t)
(autoload 'insert-text-seperator-line "e-seperators" nil t)


;; Close the compilation window if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
    ;; Specify my function (maybe I should have done a lambda function)
    (setq compilation-exit-message-function 'compilation-exit-autoclose)

(require 'ansi-color)
(require 'yaml-mode)

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))


;; -----------------------
;; Theme and customization
;; -----------------------
;; custom config
(require 'modeline)
(require 'fns)
(require 'keys)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'setimod t)
;; ---------------
;; Packages config
;; ---------------

(setq tramp-default-method "ssh")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

;; shell-pop
(use-package shell-pop
  :ensure shell-pop
  :bind (("C-c t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))


(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)



;; Install additinal themes from melpa
;; make sure to use :defer keyword
(use-package hemera-theme :ensure :defer)
(use-package nyx-theme :ensure :defer)

;;(add-to-list 'custom-theme-load-path "~/Downloads/cyberpunk-theme.el")

(load-theme 'base16-tomorrow-night-eighties t)
