;; from http://technomancy.us/153
;; Clojure stuff
(require 'package)


;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
      projectile
      furl
      ido-ubiquitous
      starter-kit
      starter-kit-lisp
      clojure-mode 
      ack-and-a-half
      cider
      auto-complete
      ac-nrepl
      smart-tab
      zenburn-theme
      buffer-move
      paredit
      erlang
      highlight-parentheses
      haskell-mode
      rainbow-delimiters
      clj-refactor
      yasnippet
      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(cua-mode t)
(delete-selection-mode 1)
(require 'clojure-mode)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'auto-complete)
(require 'buffer-move)

;; Add hooks for modes where you want it enabled, for example:
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(add-to-list 'ac-modes 'clojure-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces nil)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider.history")
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-to-list 'same-window-buffer-names "*cider*")
(setq cider-repl-display-in-current-window nil)


(add-hook 'prog-mode-hook 'paredit-mode)

(require 'ac-nrepl)
 (add-hook 'cider-mode-hook 'ac-nrepl-setup)
 (add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'cider-mode))

(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))

(real-global-auto-complete-mode t)
;; enabling frames as pop-ups:
;(setq pop-up-frames t)
(put-clojure-indent 'run* 'defun)
(put-clojure-indent 'run 'defun)
(put-clojure-indent 'fresh 'defun)
(put-clojure-indent 'conde 'defun)


(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(projectile-global-mode)
(cider-enable-on-existing-clojure-buffers)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(cider-host "localhost")
 '(cider-port "")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "f3d2144fed1adb27794a45e61166e98820ab0bbf3cc7ea708e4bf4b57447ee27" "216e6d0d3576e5c35785e68ca07b1c71f01ee4f3d80cb3b4da0ba55827bb3e5e" "d63e19a84fef5fa0341fa68814200749408ad4a321b6d9f30efc117aeaf68a2e" "e4eaeb23c81fd6c6b1796b823dbec0129d828e13da89a222901a758348db57fd" "5f946c56d7e5feaf04ea77339df7fa87300301ad450726743eca0a140e695b2c" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "965234e8069974a8b8c83e865e331e4f53ab9e74" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-font-lock turn-on-haskell-doc-mode turn-on-haskell-decl-scan imenu-add-menubar-index)))
 '(haskell-program-name "/usr/local/bin/hugs \"+.\"")
 '(hl-paren-background-colors (quote ("#3355aa" "#557733" "#335533")))
 '(hl-paren-colors (quote nil))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point nil)
 '(lua-default-application "lua")
 '(nrepl-connected-hook (quote (cider-enable-on-existing-clojure-buffers)))
 '(recentf-mode nil)
 '(send-mail-function (quote sendmail-send-it))
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
 '(font-lock-constant-face ((t (:foreground "#ddaa77" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#8cd0d3" :slant italic))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7dd6e8"))))
 '(rainbow-delimiters-depth-10-face ((t (:foreground "#ece4bf"))) t)
 '(rainbow-delimiters-depth-11-face ((t (:foreground "#ef9f7f"))) t)
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#ece47f"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#91bcf4"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ddb3b3"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#dfbca2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#89c4c7"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#c2eec6"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#ef9f7f"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9de6e8")))))

(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'expression', 'parenthesis' and 'mixed'
(set-face-attribute 'show-paren-match-face nil :underline nil :overline nil :foreground nil :background "#224455")

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)



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

(global-set-key (kbd "s-.") 'org-edit-special)
(global-set-key (kbd "s-,") 'org-edit-src-exit)

(global-set-key (kbd "C-z") 'undo) ; Ctrl+z
(global-set-key (kbd "C-S-z") 'redo) ;  Ctrl+Shift+z
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (quote [end]) 'move-end-of-line)
(global-set-key (quote [home]) 'smart-beginning-of-line)

(global-set-key (kbd "<M-s-right>") 'buf-move-right)
(global-set-key (kbd "<M-s-left>") 'buf-move-left)
(global-set-key (kbd "<M-s-up>") 'buf-move-up)
(global-set-key (kbd "<M-s-down>") 'buf-move-down)
(global-set-key (kbd "s-n") 'find-file-in-project)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

(setq scroll-step 10)
(setq scroll-conservatively 0)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))
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

(defun kill-paredit-or-region () 
  "kill region if active only or kill line normally"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'paredit-forward-delete)))

(defun killbackward-paredit-or-region () 
  "kill region if active only or kill line normally"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'paredit-backward-delete)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<kp-delete>") 'kill-paredit-or-region)
     (define-key paredit-mode-map (kbd "<backspace>") 'killbackward-paredit-or-region)
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)))

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


(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -5))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 5))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -5))
   (t (message "nil"))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 5))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -5))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 5))
   (t (message "nil"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -30))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 30))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 30))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 30))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -30))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -30))))

(global-set-key [s-S-left] 'win-resize-enlarge-horiz)
(global-set-key [s-S-right] 'win-resize-minimize-horiz)
(global-set-key [s-S-up] 'win-resize-enlarge-vert)
(global-set-key [s-S-down] 'win-resize-minimize-vert)

;(global-set-key [s-S-right] 'win-resize-enlarge-horiz)
;(global-set-key [s-S-down] 'win-resize-enlarge-horiz)
;(global-set-key [s-S-down] 'win-resize-minimize-horiz)
;(global-set-key [s-S-left] 'win-resize-enlarge-vert)



;; Get rid of popups in OSX!!
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Setup for ack-and-a-half
(setq ack-and-a-half-executable "/usr/local/bin/ack")
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-fiile 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(global-set-key (kbd "<f6>") 'highlight-regexp)
(global-set-key (kbd "M-<f6>") 'unhighlight-regexp)
(auto-fill-mode -1)
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(add-to-list 'exec-path "/usr/local/Cellar/smlnj/110.75/libexec/bin")

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B03-1")
(add-to-list 'exec-path "/usr/local/Cellar/erlang/R15B03-1/bin")
(setq erlang-man-root-dir "/usr/local/Cellar/erlang/R15B03-1/man")

;(powerline-center-theme)

(set-default-font "Inconsolata 15")

(setq ring-bell-function #'ignore)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key [f15] 'toggle-window-dedicated)

;;(require 'powerline)
;;(powerline-default-theme)

;; (defun nrepl-docsrc-handler (symbol)
;;   "Create a handler to lookup docs & source
;;   for SYMBOL."
;;   (let ((form (format "(clojure.repl/source %s)" symbol))
;;         (doc-form (format "(clojure.repl/doc %s)" symbol))
;;         (src-buffer (nrepl-popup-buffer nrepl-src-buffer t)))
;;     (with-current-buffer src-buffer
;;       (clojure-mode)
;;       (nrepl-popup-buffer-mode +1))
;;     (nrepl-send-string doc-form
;;                        (nrepl-popup-eval-out-handler src-buffer)
;;                        nrepl-buffer-ns
;;                        (nrepl-current-tooling-session))
;;     (nrepl-send-string form
;;                        (nrepl-popup-eval-out-handler src-buffer)
;;                        nrepl-buffer-ns
;;                        (nrepl-current-tooling-session))))

;; (defun nrepl-docsrc (query)
;;   "Open a window with the source for the given QUERY.
;; Defaults to the symbol at point.  With prefix arg or no symbol
;; under point, prompts for a var."
;;   (interactive "P")
;;   (nrepl-read-symbol-name "Symbol: " 'nrepl-docsrc-handler query))


;; (defun clojure-cheatsheet/lookup-docsrc
;;   (symbol)
;;   (if (nrepl-current-connection-buffer)
;;       (nrepl-docsrc-handler symbol)
;;     (error "nREPL not connected!")))

;; (defun clojure-cheatsheet/item-to-helm-source
;;   (item)
;;   (let ((heading (car item))
;; 	(symbols (cdr item)))
;;     `((name . ,heading)
;;       (candidates ,@symbols)
;;       (match . ((lambda (candidate)
;; 		  (helm-mp-3-match (format "%s %s" candidate ,heading)))))
;;       (action . (("Lookup Docs & Source" . clojure-cheatsheet/lookup-docsrc)
;;       		 ("Lookup Docs" . clojure-cheatsheet/lookup-doc)
;; 		 ("Lookup Source" . clojure-cheatsheet/lookup-src))))))

;; (defvar helm-source-clojure-cheatsheet
;;  (mapcar 'clojure-cheatsheet/item-to-helm-source
;; 	 (clojure-cheatsheet/group-by-head
;; 	  (clojure-cheatsheet/flatten
;; 	   (clojure-cheatsheet/propagate-headings clojure-cheatsheet-hierarchy)))))

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (sh . t)
   (lisp . t)
   (ditaa . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)


(declare-function nrepl-send-string-sync "ext:nrepl" (code &optional ns))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (require 'nrepl)
  (with-temp-buffer
    (insert (org-babel-expand-body:clojure body params))
    ((lambda (result)
       (let ((result-params (cdr (assoc :result-params params))))
         (if (or (member "scalar" result-params)
                 (member "verbatim" result-params))
             result
           (condition-case nil (org-babel-script-escape result)
             (error result)))))
     (plist-get (nrepl-send-string-sync
                 (buffer-substring-no-properties (point-min) (point-max))
                 (cdr (assoc :package params)))
                :value))))


(add-hook 'clojure-mode-hook
          '(lambda ()
             (font-lock-add-keywords
                nil
                '(("(\\(\\w+[/]\\w+\\)"
                   (1 font-lock-function-name-face))))))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "s-r")))

(global-set-key (kbd "<M-s-right>") 'buf-move-right)

(yas-global-mode 1)
