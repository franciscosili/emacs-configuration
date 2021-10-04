;; -- emacs keys

;; urxvt's keycodes fixes
(global-set-key (kbd "\033[1;5D") 'backward-word)
(global-set-key (kbd "\033[1;5C") 'forward-word)

(add-hook 'term-setup-hook
  '(lambda ()
     (define-key function-key-map "\e[1;3A" [M-up])
     (define-key function-key-map "\e[1;3B" [M-down])
     (define-key function-key-map "\e[1;3C" [M-right])
     (define-key function-key-map "\e[1;3D" [M-left])))

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Move windows
;; (global-set-key (kbd "S-right") 'windmove-right)
;; (global-set-key (kbd "S-left")  'windmove-left)
(global-set-key (kbd "<select>")    'windmove-up)
;; (global-set-key (kbd "S-down")  'windmove-down)

;; move window borders
(global-set-key (kbd "M-[") 'move-border-left)
(global-set-key (kbd "M-]") 'move-border-right)

;; commenting region or line
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)

;; remap C-a to smarter-move-beginning-of-line
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; others
(global-set-key (kbd "C-l")      'goto-line)
(global-set-key (kbd "C-h")      'delete-backward-char)
(global-set-key (kbd "\C-x\C-u") 'shell)
(global-set-key (kbd "M-SPC")    'multi-line-just-one-space)
(global-set-key (kbd "C-c k")    'kill-other-buffers)
(global-set-key (kbd "C-c v")    'switch-src-header)
(global-set-key (kbd "C-c t")    'mo-toggle-identifier-naming-style)
(global-set-key (kbd "C-\"")     'toggle-quotes)
(global-set-key (kbd "C-c +")    'increment-number-at-point)
(global-set-key (kbd "C-c r")    'rename-this-file-and-buffer)
(global-set-key (kbd "C-c d")    'delete-this-file)
(global-set-key (kbd "C-x c")    'compile)
(global-set-key (kbd "M-s e")    'sudo-edit)
(global-set-key (kbd "C-c s")    'win-swap)
(global-set-key (kbd "C-c C-v")  'atl-switch-src)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(define-key global-map (kbd "C-c M-t") 'center-text-mode)

;; clatex
(global-set-key (kbd "C-c c")
                (lambda ()
                  (interactive)
                  (shell-command (concat "clatex -f " buffer-file-name))))

;; python script
(global-set-key (kbd "C-c p")
                (lambda ()
                  (interactive)
                  (shell-command (concat "python " buffer-file-name))))

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


(provide 'keys)
