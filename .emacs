; Packages

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(package-initialize)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

; Clipboard

(global-set-key (kbd "C-c M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c C-w") 'clipboard-kill-region)
(global-set-key (kbd "C-c C-y") 'clipboard-yank)

; Compilation

; (add-to-list 'compilation-error-regexp-alist
;              '("^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)" 1 2 (3 . 4)))

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(setq-default compile-command "make --print-directory --keep-going")
(setq-default compilation-scroll-output t)

; Navigation
(global-set-key (kbd "C-x p") 'ffap)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)

; File Extensions

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.as\\'" . javascript-mode))

; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; C and C++

(add-hook 'c-mode-hook 'my-c-mode-hook)
(defun my-c-mode-hook ()
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (global-auto-complete-mode))

; C#

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(defun my-csharp-mode-hook ()
  (c-set-offset 'inline-open 0))

; CSS/Less

(defun xah-syntax-color-hex ()
  "Syntax color text of the form \"#ff1100\" in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\(#\\)[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 1)
          (match-end 1)
          'face (let ((color (match-string-no-properties 0))) (list :background color :foreground color)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'css-mode-hook 'xah-syntax-color-hex)

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
(global-font-lock-mode 1)
(menu-bar-mode 0)
(show-paren-mode)
(ido-mode 1)
(global-subword-mode 1)
(global-git-gutter-mode +1)

; Whitespace

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(global-visual-line-mode 1)
(setq-default adaptive-wrap-extra-indent 3)

(add-hook 'visual-line-mode-hook 'my-enable-adaptive-wrap)
(defun my-enable-adaptive-wrap ()
  (adaptive-wrap-prefix-mode 1))

; Buffers

(defun revert-all-buffers ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t)))))

; Frames

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(set-face-background 'vertical-border "transparent")
(set-display-table-slot standard-display-table 0 (make-glyph-code ?…))

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(global-set-key (kbd "C-x |") 'window-toggle-split-direction)

; Keys

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x s") 'sort-lines)
(global-set-key (kbd "DEL") 'backward-delete-char)
(global-set-key (kbd "M-]") 'delete-matching)
(global-set-key (kbd "M-s t") 'tags-search)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c R") 'revert-buffer)
(global-set-key (kbd "C-x [") 'shrink-window)
(global-set-key (kbd "C-x ]") 'enlarge-window)
(global-set-key (kbd "C-x ]") 'enlarge-window)
(global-set-key (kbd "C-c d h") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-c d r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c d s") 'git-gutter:stage-hunk)

; Configuration

(set-default-font "Source Code Pro 14")
(setq-default initial-scratch-message "")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

; ido

(require 'flx-ido)
(flx-ido-mode t)

; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-comment-tag ((t (:foreground "blue"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(error ((t (:foreground "brightred" :weight bold))))
 '(cperl-array-face ((t nil)))
 '(cperl-nonoverridable-face ((t nil)))
 '(font-lock-builtin-face ((t (:foreground "brightblack"))))
 '(font-lock-comment-face ((t (:foreground "slateblue"))))
 '(font-lock-constant-face ((t (:foreground "black"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "black"))))
 '(font-lock-keyword-face ((t (:foreground "brightblack"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "black"))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(link-visited ((t (:inherit link :foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "white"))))
 '(shadow ((t (:foreground "red"))))
 '(success ((t (:foreground "brightgreen" :weight bold))))

 '(git-gutter:added ((t (:background "green" :foreground "brightwhite" :weight bold))))
 '(git-gutter:deleted ((t (:background "red" :foreground "brightwhite" :weight bold))))
 '(git-gutter:modified ((t (:background "blue" :foreground "brightwhite" :weight bold))))
 '(hl-line ((t nil)))
 '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey95" :foreground "grey40" :box (:line-width -1 :color "grey75") :weight light))))
 '(whitespace-indentation ((t (:foreground "lightgray"))))
 '(whitespace-line ((t nil)))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:inherit nil)))))

; Custom

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(c-auto-align-backslashes nil)
 '(c-backspace-function (quote backward-delete-char))
 '(dired-use-ls-dired nil)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-faces nil)
 '(linum-format (quote "%7d|"))
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "k&r") (csharp-mode . "linux"))))
 '(git-gutter:modified-sign "*")
 '(git-gutter:separator-sign nil)
 '(global-hl-line-sticky-flag nil)
 '(global-whitespace-mode nil)
 '(hl-line-sticky-flag t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(mode-line-format
   (quote
    ("%b:%l:%c (%m) [%+] "
     (:eval
      (make-string 1000 9472)))))
 '(tab-width 4)
 '(truncate-lines t)
 '(whitespace-display-mappings
   (quote
    ((space-mark 32 [183] [46])
     (space-mark 160 [164] [95])
     (newline-mark 10 [36 10])
     (tab-mark 9 [183 9] [187 9] [92 9]))))
 '(whitespace-style (quote (face tabs spaces tab-mark))))
