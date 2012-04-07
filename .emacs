;; from http://technomancy.us/153
;; Clojure stuff
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(            
      rainbow-delimiters
      marmalade
      furl
      ido-ubiquitous
      starter-kit
      starter-kit-lisp
      slime
      clojure-mode 
      auto-complete
      ;slime-fuzzy
      ac-slime
      ;slime-clj
      ;slime-repl
      ;php-mode
      smart-tab
      undo-tree
      zenburn-theme
      ;tabbar-ruler
      ;tabbar
      ; slime      ;embedded inside swank-clojure-1.4.0-SNAPSHOT.jar\swank\payload
      ; slime-repl ;embedded inside swank-clojure-1.4.0-SNAPSHOT.jar\swank\payload
      paredit
))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'clojure-mode)
(require 'slime)
(require 'rainbow-delimiters)
;; Add hooks for modes where you want it enabled, for example:
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; enabling frames as pop-ups:
;(setq pop-up-frames t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "965234e8069974a8b8c83e865e331e4f53ab9e74" default)))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(show-paren-mode t)
 '(standard-indent 2)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(visible-bell nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((t (:foreground "#cc8844"))))
 '(rainbow-delimiters-depth-1-face ((((background light)) (:foreground "black"))))
 '(rainbow-delimiters-depth-2-face ((((background light)) (:foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((((background light)) (:foreground "red"))))
 '(rainbow-delimiters-depth-4-face ((((background light)) (:foreground "cyan"))))
 '(rainbow-delimiters-depth-5-face ((((background light)) (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((((background light)) (:foreground "brown"))))
 '(rainbow-delimiters-depth-7-face ((((background light)) (:foreground "pink"))))
 '(rainbow-delimiters-depth-8-face ((((background light)) (:foreground "blue"))))
 '(rainbow-delimiters-unmatched-face ((((background light)) (:foreground "grey")))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

;; Disable startup screen
(setq inhibit-startup-screen t)

(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-z") 'undo) ; Ctrl+z
(global-set-key (kbd "C-S-z") 'redo) ;  Ctrl+Shift+z
(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (quote [end]) 'move-end-of-line)
(global-set-key (quote [home]) 'smart-beginning-of-line)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

;; dragging lines up and down with M-<up> and M-<down>
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<M-up>") 'move-line-up)

(require 'smart-tab)
(global-smart-tab-mode 1)

(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)

(delete-selection-mode t)

(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'expression', 'parenthesis' and 'mixed'
(set-face-attribute 'show-paren-match-face nil 
        :weight 'normal :underline nil :overline nil :slant 'normal)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (query-replace "" ""))

(defun newline-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (query-replace "" "
"))

(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key "\M-i" 'ido-goto-symbol) ; or any key you see fit


;; Window movement
(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)

(set-default-font "Inconsolata 13")
