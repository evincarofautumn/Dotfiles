; Packages

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; Directory Locals

(setq-default enable-dir-locals nil)

; Input Methods

(with-temp-buffer
  (activate-input-method "TeX")
  (let ((quail-current-package (assoc "TeX" quail-package-alist)))
    (quail-define-rules
     ((append . t))
     ("\\parr"    #x214B)  ; ⅋
     ("\\from"    #x2190)  ; ←
     ("\\From"    #x21D0)  ; ⇐
     ("\\To"      #x21D2)  ; ⇒
     ("\\tofrom"  #x2194)  ; ↔
     ("\\llbrack" #x27E6)  ; ⟦
     ("\\rrbrack" #x27E7)  ; ⟧
     ("\\llangle" #x27EA)  ; ⟪
     ("\\rrangle" #x27EB)  ; ⟫
     ("\\llbrace" #x2983)  ; ⦃
     ("\\rrbrace" #x2984)  ; ⦄
     ("\\llparen" #x2985)  ; ⦅
     ("\\rrparen" #x2986)  ; ⦆
     ("\\prop"    #x2237)  ; ∷
     ("\\Star"    #x2B51)  ; ⭑

     ; ASCII
     ("\\hm"       #x002D) ; - hyphen-minus
     ("\\us"       #x005F) ; _ low line

     ; IPA
     ("\\scripta"  #x0251) ; ɑ
     ("\\openo"    #x0254) ; ɔ
     ("\\sci"      #x026A) ; ɪ
     ("\\bari"     #x0268) ; ɨ
     ("\\scoe"     #x0276) ; ɶ
     ("\\baru"     #x0289) ; ʉ
     ("\\schwa"    #x0259) ; ə
     ("\\opene"    #x025B) ; ɛ
     ("\\esh"      #x0283) ; ʃ
     ("\\ezh"      #x0292) ; ʒ
     ("\\U"        #x0361) ; ◌͡◌ double inverted breve
     ("\\dotcirc"  #x25CC) ; ◌ dotted circle

     ; Box Drawings

       ; Corners & Sides           Vertical      Horizontal

         ; Single
         ("\\tl"       #x250C) ; ┌ single top    single left
         ("\\tc"       #x252C) ; ┬ single top    single center
         ("\\tr"       #x2510) ; ┐ single top    single right
         ("\\ml"       #x251C) ; ├ single middle single left
         ("\\mc"       #x253C) ; ┼ single middle single center
         ("\\mr"       #x2524) ; ┤ single middle single right
         ("\\bl"       #x2514) ; └ single bottom single left
         ("\\bc"       #x2534) ; ┴ single bottom single center
         ("\\br"       #x2518) ; ┘ single bottom single right

         ; Double
         ("\\ttll"     #x2554) ; ╔ double top    double left
         ("\\ttcc"     #x2566) ; ╦ double top    double center
         ("\\ttrr"     #x2557) ; ╗ double top    double right
         ("\\mmll"     #x2560) ; ╠ double middle double left
         ("\\mmcc"     #x256C) ; ╬ double middle double center
         ("\\mmrr"     #x2563) ; ╣ double middle double right
         ("\\bbll"     #x255A) ; ╚ double bottom double left
         ("\\bbcc"     #x2569) ; ╩ double bottom double center
         ("\\bbrr"     #x255D) ; ╝ double bottom double right

         ; Joins
         ("\\ttl"      #x2552) ; ╒ double top    single left
         ("\\ttc"      #x2564) ; ╤ double top    single center
         ("\\ttr"      #x2555) ; ╕ double top    single right
         ("\\mml"      #x255E) ; ╞ double middle single left
         ("\\mmc"      #x256A) ; ╪ double middle single center
         ("\\mmr"      #x2561) ; ╡ double middle single right
         ("\\bbl"      #x2558) ; ╘ double bottom single left
         ("\\bbc"      #x2567) ; ╧ double bottom single center
         ("\\bbr"      #x255B) ; ╛ double bottom single right

         ("\\tll"      #x2553) ; ╓ single top    double left
         ("\\tcc"      #x2565) ; ╥ single top    double center
         ("\\trr"      #x2556) ; ╖ single top    double right
         ("\\mll"      #x255F) ; ╟ single middle double left
         ("\\mcc"      #x256B) ; ╫ single middle double center
         ("\\mrr"      #x2562) ; ╢ single middle double right
         ("\\bll"      #x2559) ; ╙ single bottom double left
         ("\\bcc"      #x2568) ; ╨ single bottom double center
         ("\\brr"      #x255C) ; ╜ single bottom double right

       ; Lines
         ; Single
         ("\\lh"       #x2500) ; ─ single middle
         ("\\lv"       #x2502) ; │               single center
         ; Double
         ("\\lhh"      #x2550) ; ═ double middle
         ("\\lvv"      #x2551) ; ║               double center
     )))

; Clipboard

(defun pbcopy (text)
  (interactive "M\np")
  (let* ((PIPE nil)
         (process-connection-type PIPE)
         (command "pbcopy")
         (buffer "*Messages*")
         (process (start-process command buffer command)))
    (process-send-string process text)
    (process-send-eof process)))

(defun pbpaste (&optional prefix)
  (interactive "p")
  (let ((result (shell-command-to-string "pbpaste")))
    (if prefix
        (insert result)
      result)))

(when (eq system-type 'darwin)
  (setq interprogram-cut-function 'pbcopy)
  (setq interprogram-paste-function 'pbpaste)
  nil)

(global-set-key (kbd "C-c M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c C-w") 'clipboard-kill-region)
(global-set-key (kbd "C-c C-y") 'clipboard-yank)

; Compilation

; (add-to-list 'compilation-error-regexp-alist
;              '("^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)" 1 2 (3 . 4)))
(add-hook 'compilation-mode-hook 'haskell-compilation-mode-hook)
(defun haskell-compilation-mode-hook ()
  (add-to-list 'compilation-error-regexp-alist
               'ghc-bound-at-info)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ghc-bound-at-info
                 "(bound at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
                 1 2 3 0))
  (add-to-list 'compilation-error-regexp-alist
               'ghc-defined-at-info)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ghc-defined-at-info
                 "-- Defined at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3 0))
  (add-to-list 'compilation-error-regexp-alist
               'ghc-import-info)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ghc-import-info
                 "in the import of `[0-9A-Za-z\.]+' (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
                 1 2 3 0)))

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(setq-default compile-command "make --print-directory --keep-going")
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(defun ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

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
  ; (global-auto-complete-mode)
  )

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

(setq-default fill-column 64)
(global-display-fill-column-indicator-mode)

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

; Sorting

; Aped from sort-lines.
(defun sort-words (reverse beg end)
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "*P\nr")
  (let ((word "\\w+")
        (whole-record "\\&"))
    (sort-regexp-fields reverse word whole-record beg end)))

; Line Numbering

(setq-default linum-suffix "|")
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((buffer-height (count-lines (point-min) (point-max)))
         (width (length (number-to-string buffer-height)))
         (linum-format (concat "%" (number-to-string width) "d" linum-suffix)))
    ad-do-it))

; Global Minor Modes

(column-number-mode)
(show-paren-mode)
(setq-default show-paren-delay 0)
(global-font-lock-mode 1)
(menu-bar-mode 0)
(ido-mode 1)
(global-subword-mode 1)
(global-set-key (kbd "C-c s") 'subword-mode)
(require 'git-gutter)
(global-git-gutter-mode +1)

; Whitespace

(setq-default search-whitespace-regexp nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(global-visual-line-mode nil)

(require 'adaptive-wrap)
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

(defun rename-file-and-buffer (new-name)
  "Renames an open file."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file." name)
      (if (get-buffer new-name)
          (message "Already visiting a file named '%s'." new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

; Mode Line

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled. "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(define-globalized-minor-mode global-hidden-mode-line-mode
  hidden-mode-line-mode
  (lambda () (hidden-mode-line-mode 1)))

(global-hidden-mode-line-mode 1)

(require 'auto-dim-other-buffers)
(auto-dim-other-buffers-mode t)

; (defun highlight-selected-window ()
;   "Dim background of deselected windows."
;   (walk-windows (lambda (window)
;                   (with-current-buffer (window-buffer window)
;                     (if (eq window (selected-window))
;                         (buffer-face-set 'default)
;                       (buffer-face-set '(:background "#eee"))))))
;   (buffer-face-set 'default)
;   (force-window-update (current-buffer)))
;
; (add-hook 'buffer-list-update-hook 'highlight-selected-window)
; (add-hook 'focus-out-hook 'highlight-selected-window)
; (add-hook 'focus-in-hook 'highlight-selected-window)

; Frames

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(set-face-background 'vertical-border "unspecified-bg")
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))

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
(global-unset-key (kbd "C-x s"))
(global-set-key (kbd "C-x s l") 'sort-lines)
(global-set-key (kbd "C-x s w") 'sort-words)
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
(global-set-key (kbd "C-c d n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c d p") 'git-gutter:previous-hunk)

; Configuration

(setq-default initial-scratch-message "")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)

; ido

(require 'flx-ido)
(flx-ido-mode t)
(ido-everywhere 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

; Mode Line

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled. "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)
; (hidden-mode-line-mode t)

(define-globalized-minor-mode global-hidden-mode-line-mode
  hidden-mode-line-mode
  (lambda () (hidden-mode-line-mode 1)))

(global-hidden-mode-line-mode 1)

(require 'auto-dim-other-buffers)
(auto-dim-other-buffers-mode t)

; (defun highlight-selected-window ()
;   "Dim background of deselected windows."
;   (walk-windows (lambda (window)
;                   (with-current-buffer (window-buffer window)
;                     (if (eq window (selected-window))
;                         (buffer-face-set 'default)
;                       (buffer-face-set '(:background "#eee"))))))
;   (buffer-face-set 'default)
;   (force-window-update (current-buffer)))
;
; (add-hook 'buffer-list-update-hook 'highlight-selected-window)
; (add-hook 'focus-out-hook 'highlight-selected-window)
; (add-hook 'focus-in-hook 'highlight-selected-window)

; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#efefef"))))
 '(cperl-array-face ((t nil)))
 '(cperl-nonoverridable-face ((t nil)))
 '(custom-comment-tag ((t (:foreground "blue"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(error ((t (:foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "brightblack"))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-constant-face ((t (:foreground "black"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "black"))))
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(font-lock-string-face ((t (:foreground "gray55"))))
 '(font-lock-type-face ((t (:foreground "black"))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(git-gutter:added ((t (:background "green" :foreground "brightwhite" :weight bold))))
 '(git-gutter:deleted ((t (:background "red" :foreground "brightwhite" :weight bold))))
 '(git-gutter:modified ((t (:background "blue" :foreground "brightwhite" :weight bold))))
 '(hl-line ((t nil)))
 '(link-visited ((t (:inherit link :foreground "magenta"))))
 '(minibuffer-prompt ((t (:foreground "brightblack"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey95" :foreground "grey40" :box (:line-width -1 :color "grey75") :weight light))))
 '(shadow ((t (:foreground "red"))))
 '(success ((t (:foreground "green"))))
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
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(c-auto-align-backslashes nil)
 '(c-backspace-function 'backward-delete-char)
 '(c-basic-offset 4)
 '(c-default-style '((c-mode . "k&r") (csharp-mode . "linux")))
 '(c-offsets-alist
   '((arglist-intro . +)
     (arglist-cont . 0)
     (arglist-cont-nonempty . 0)
     (arglist-close . 0)))
 '(c-syntactic-indentation t)
 '(custom-safe-themes
   '("e98ed3eea2e9585ce0989043c7c03ee9248dc69e4e4ac5d9807fe163e04aade3" default))
 '(dired-use-ls-dired nil)
 '(git-gutter:modified-sign "*")
 '(git-gutter:separator-sign nil)
 '(global-hl-line-sticky-flag nil)
 '(grep-command "grep --color -nH --null -e")
 '(grep-files-aliases
   '(("all" . "* .[!.]* ..?*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("hs" . "*.hs *.lhs")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")))
 '(grep-find-template
   "find <D> <X> -type f <F> -exec grep <C> -n --null -e <R> \\{\\} +")
 '(grep-use-null-device nil)
 '(hl-line-sticky-flag t)
 '(package-selected-packages
   '(yaml-mode s nix-mode haskell-mode git-gutter git-gutter+ flycheck flx-ido fill-column-indicator company auto-dim-other-buffers adaptive-wrap))
 '(sentence-end-double-space nil))
(put 'set-goal-column 'disabled nil)
