; Packages

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq
 package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; Clipboard

(global-set-key (kbd "C-c M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c C-w") 'clipboard-kill-region)
(global-set-key (kbd "C-c C-y") 'clipboard-yank)

; Compilation

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(setq-default compile-command "make")
(setq compilation-scroll-output 1)

; Navigation
(global-set-key (kbd "C-x p") 'ffap)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)

; File Extensions

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.as\\'" . javascript-mode))

; Kitten

(require 'kitten-mode)

; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; C and C++

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-add-style "mine"
 '((c-offsets-alist . ((innamespace . 0)))))
(add-hook
 'c-mode-common-hook
 '(lambda ()
    (c-set-style "mine")
    (setq tab-width 2)
    (setq c-basic-offset tab-width)))

; Fill Column

(setq-default fill-column 60)
(require 'fill-column-indicator)
(setq-default fci-rule-column 60)

; UUIDs

(defun uuidgen (&optional unused)
  (interactive)
  (insert (shell-command-to-string "uuidgen | tr 'a-z-' 'A-Z_'")))

; Brackets

(defun delete-matching ()
  "Deletes the bracket character under point, as well as the one matching it."
  (interactive)
  (cond
   ((looking-at "\\s\(")
    (save-excursion (forward-sexp 1) (delete-char -1))
    (delete-char 1))
   ((save-excursion (backward-char 1) (looking-at "\\s\)"))
    (save-excursion (backward-sexp 1) (delete-char 1))
    (delete-char -1))))

(defun wrap-region (start end left right)
  "Wraps a region with the given prefix and suffix."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (insert left)
    (goto-char (1+ end))
    (insert right)))

(defun parenthesize-region (start end)
  "Wraps a region with parentheses."
  (interactive "r")
  (wrap-region start end "(" ")"))

(defun bracket-region (start end)
  "Wraps a region with square brackets."
  (interactive "r")
  (wrap-region start end "[" "]"))

(defun brace-region (start end)
  "Wraps a region with curly braces."
  (interactive "r")
  (wrap-region start end "{" "}"))

(global-set-key (kbd "M-[ (") 'parenthesize-region)
(global-set-key (kbd "M-[ [") 'bracket-region)
(global-set-key (kbd "M-[ {") 'brace-region)
(global-set-key (kbd "M-]") 'delete-matching)

; Line Numbering

(setq-default linum-suffix "|")
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((buffer-height (count-lines (point-min) (point-max)))
         (width (length (number-to-string buffer-height)))
         (linum-format (concat "%" (number-to-string width) "d" linum-suffix)))
    ad-do-it))

; Global Minor Modes

(show-paren-mode)
(column-number-mode)
(setq-default show-paren-delay 0)
(menu-bar-mode -1)
(ido-mode 1)
(global-subword-mode 1)
(global-font-lock-mode 0)

; Whitespace

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(global-visual-line-mode 1)
(setq-default adaptive-wrap-extra-indent 3)

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x s") 'sort-lines)
(global-set-key (kbd "DEL") 'backward-delete-char)

; Configuration

(set-default-font "Source Code Pro 14")
(setq-default initial-scratch-message "")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-comment-tag ((t (:foreground "blue"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(error ((t (:foreground "brightred" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "slateblue"))))
 '(font-lock-function-name-face ((t (:foreground "black"))))
 '(font-lock-keyword-face ((t (:foreground "brightblack"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "black"))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(link-visited ((t (:inherit link :foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "white"))))
 '(shadow ((t (:foreground "red"))))
 '(success ((t (:foreground "brightgreen" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-backspace-function (quote backward-delete-char))
 '(dired-use-ls-dired nil)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-faces nil)
 '(linum-format (quote "%7d|")))
