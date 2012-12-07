; Clipboard

(global-set-key (kbd "C-c M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c C-w") 'clipboard-kill-region)
(global-set-key (kbd "C-c C-y") 'clipboard-yank)

; C++

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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

; Whitespace

(setq-default indent-tabs-mode nil)
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

; (font-lock-add-keywords 'haskell-mode
;  '(("\\(\\\\\\)\\s-" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?λ) nil))
; 
;    ("[^[:digit:][:space:]]\\(0\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₀) nil))
;    ("[^[:digit:][:space:]]\\(1\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₁) nil))
;    ("[^[:digit:][:space:]]\\(2\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₂) nil))
;    ("[^[:digit:][:space:]]\\(3\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₃) nil))
;    ("[^[:digit:][:space:]]\\(4\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₄) nil))
;    ("[^[:digit:][:space:]]\\(5\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₅) nil))
;    ("[^[:digit:][:space:]]\\(6\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₆) nil))
;    ("[^[:digit:][:space:]]\\(7\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₇) nil))
;    ("[^[:digit:][:space:]]\\(8\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₈) nil))
;    ("[^[:digit:][:space:]]\\(9\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?₉) nil))
; 
;    ("\\sw\\('\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?′) nil))
;    ("\\sw\\(''\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?″) nil))
;    ("\\sw\\('''\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?‴) nil))
;    ("\\sw\\(''''\\)\\Sw" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?⁗) nil))
; 
;    ("\\S_\\(->\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?→) nil))
;    ("\\S_\\(<-\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?←) nil))
; 
;    ("\\S_\\(=>\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?⇒) nil))
;    ("\\S_\\(<=\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?≤) nil))
;    ("\\S_\\(>=\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?≥) nil))
; 
;    ("\\S_\\(\\*\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?×) nil))
;    ("\\S_<\\(\\*\\)>\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∗) nil))
;    ("\\S_\\(\\*\\)>\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∗) nil))
;    ("\\S_<\\(\\*\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∗) nil))
; 
;    ("\\S_\\(<\\)\\*\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?‹) nil))
;    ("\\S_\\*\\(>\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?›) nil))
;    ("\\S_\\(<\\)\\$\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?‹) nil))
;    ("\\S_\\$\\(>\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?›) nil))
; 
;    ("\\S_\\(>>\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?») nil))
;    ("\\S_\\(>>=\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?↣) nil))
;    ("\\S_\\(=<<\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?↢) nil))
; 
;    ("\\s-\\(\\.\\)\\s-" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∘) nil))
;    ("[^[:digit:][:space:]]\\(\\.\\)[^[:digit:][:space:]]" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?·) nil))
; 
;    ("\\S_\\(\\-\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?−) nil))
;    ("\\S_\\(&&\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∧) nil))
;    ("\\S_\\(||\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∨) nil))
;    ("\\S_\\(\\+\\+\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?⧺) nil))
;    ("\\S_<\\(\\+\\+\\)>\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?⧺) nil))
; 
;    ("\\S_\\(==\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?≡) nil))
;    ("\\S_\\(/=\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?≢) nil))
; 
;    ("\\S_\\(--\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?—) nil))
;    ("\\S_\\(\\.\\.\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?…) nil))
; 
;    ("\\S_\\(::\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?∷) nil))
; 
;    ("\\S_\\(<\\)\\s_+>\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?‹) nil))
;    ("\\S_<\\s_+\\(>\\)\\S_" 0 (progn (compose-region (match-beginning 1) (match-end 1) ?›) nil))))
