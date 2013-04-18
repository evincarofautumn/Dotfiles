; Packages

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

(setq-default compile-command "make")
(setq compilation-scroll-output 1)

; File Extensions

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.as\\'" . javascript-mode))

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

(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-size-change-functions 'auto-fci-mode)
(defun auto-fci-mode (&optional unused)
  (if (> (frame-width) 80)
      (fci-mode 1)
    (fci-mode 0)))

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

(custom-set-variables '(linum-format 'dynamic))
(custom-set-variables '(linum-format (quote "%7d|")))
(setq-default linum-suffix "|")
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((buffer-height (count-lines (point-min) (point-max)))
         (width (length (number-to-string buffer-height)))
         (linum-format (concat "%" (number-to-string width) "d" linum-suffix)))
    ad-do-it))

(global-linum-mode t)

; Global Minor Modes

(show-paren-mode)
(column-number-mode)
(setq-default show-paren-delay 0)
(menu-bar-mode -1)

; Whitespace

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x M-s") 'sort-lines)

; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(setq haskell-indent-offset 2)

; Configuration

(set-frame-font "DejaVu Sans Mono 12")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-comment-tag ((t (:foreground "blue"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(error ((t (:foreground "brightred" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "brightred"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(link-visited ((t (:inherit link :foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(shadow ((t (:foreground "red"))))
 '(success ((t (:foreground "brightgreen" :weight bold)))))
