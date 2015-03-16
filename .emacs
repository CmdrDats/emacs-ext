;; from http://technomancy.us/153
;; Clojure stuff
(require 'package)


(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
      ;;dash
      ;;furl
      ido-ubiquitous
      starter-kit
      starter-kit-lisp
      clojure-mode 
      ack-and-a-half
      cider
      ;;auto-complete
      ac-cider
      ;;ac-cider-compliment
      smart-tab
      zenburn-theme
      buffer-move
      paredit
      erlang
      highlight-parentheses
      haskell-mode
      rainbow-delimiters
      clj-refactor
      php-mode
      web-mode
      flx-ido
      git-gutter
      ;;yasnippet
      ;;color-identifiers-mode
      ;;fill-column-indicator
      ;;powerline
      projectile
      gnuplot-mode
      ;;company
      ;;company-cider
      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(cua-mode t)
(delete-selection-mode 1)

;;(require 'fill-column-indicator)
;;(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;  (global-fci-mode 1)

;;(add-hook 'after-change-major-mode-hook 'fci-mode)

;;(require 'powerline)
;;(setq powerline-arrow-shape 'arrow)




(require 'clojure-mode)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'auto-complete)
(require 'buffer-move)

;; Add hooks for modes where you want it enabled, for example:
;;(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(add-to-list 'ac-modes 'clojure-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)

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
(setq cider-known-endpoints '(("inhouse" "dev" "4005") ("cljserver" "dev" "4007") ("printserv" "dev" "4008") ("live" "192.168.1.11" "4005")))

(add-hook 'prog-mode-hook 'paredit-mode)
(add-hook 'php-mode-hook 'disable-paredit-mode)
(add-hook 'javascript-mode-hook 'disable-paredit-mode)
(add-hook 'web-mode-hook 'disable-paredit-mode)
(add-hook 'web-mode-hook 'electric-pair-mode)
(add-hook 'web-mode-hook 'turn-off-auto-fill)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

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

;;(load-theme 'solarized-dark t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(cider-host "localhost")
 '(cider-port "")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "f3d2144fed1adb27794a45e61166e98820ab0bbf3cc7ea708e4bf4b57447ee27" "216e6d0d3576e5c35785e68ca07b1c71f01ee4f3d80cb3b4da0ba55827bb3e5e" "d63e19a84fef5fa0341fa68814200749408ad4a321b6d9f30efc117aeaf68a2e" "e4eaeb23c81fd6c6b1796b823dbec0129d828e13da89a222901a758348db57fd" "5f946c56d7e5feaf04ea77339df7fa87300301ad450726743eca0a140e695b2c" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "965234e8069974a8b8c83e865e331e4f53ab9e74" default)))
 '(fci-dash-pattern 0.6)
 '(fci-handle-line-move-visual nil)
 '(fci-handle-truncate-lines nil)
 '(fci-rule-color "gray11")
 '(fci-rule-use-dashes t)
 '(fill-column 100)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-font-lock turn-on-haskell-doc-mode turn-on-haskell-decl-scan imenu-add-menubar-index)))
 '(haskell-program-name "/usr/local/bin/hugs \"+.\"")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#002b36" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(hl-paren-background-colors (quote ("#3355aa" "#557733" "#335533")))
 '(hl-paren-colors (quote nil))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point nil)
 '(lua-default-application "lua")
 '(magit-diff-use-overlays nil)
 '(magit-gitk-executable "/usr/local/bin/gitk")
 '(magit-use-overlays nil)
 '(nrepl-connected-hook (quote (cider-enable-on-existing-clojure-buffers)))
 '(recentf-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(standard-indent 2)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(visible-bell nil)
 '(web-mode-code-indent-offset 4)
 '(web-mode-markup-indent-offset 4)
 '(web-mode-script-padding 4)
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#657b83" :weight bold :slant normal))))
 '(font-lock-constant-face ((t (:foreground "#ddaa77" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#8cd0d3" :slant italic))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#383838"))) t)
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#383838"))) t)
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
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9de6e8"))))
 '(web-mode-block-face ((t (:background "gray22")))))



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
(global-set-key (kbd "<M-down>") 'up-list)
(global-set-key (kbd "<M-up>") 'backward-up-list)


(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

(setq scroll-step 10)
(setq scroll-conservatively 0)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))


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

(require 'ob)
(setq org-ditaa-jar-path "~/ditaa0_9.jar")
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

;; (defvar clojure-brace-face		'clojure-brace-face
;;   "Face used for clojure braces")
;; (defface clojure-brace-face
;;   '((t :background "#992222" :weight bold))
;;   "Face used for clojure braces"
;;   :tag "Clojure brace face"
;;   :group 'clojure
;;   )
;; (font-lock-add-keywords 'clojure-mode '(("\\([\\{\\}]+\\)" 1 clojure-brace-face append)))


(add-hook 'clojure-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '(("(\\(\\w+[/]\\w+\\)"
                 (1 font-lock-function-name-face))))
             (font-lock-add-keywords
              nil
              '(("\\(\\w+[/]\\w+\\)"
                 (1 font-lock-type-face))))
             (font-lock-add-keywords
              nil
              '(("\\(ent[/]\\w+\\)"
                 (1 font-lock-warning-face t))))
             (font-lock-add-keywords
              nil
              '(("SELECT\\|FROM\\|WHERE\\|LEFT JOIN\\|GROUP BY\\|ORDER BY\\|LIMIT\\|HAVING\\|AND\\|OR\\|IN\\|DESC\\|ASC\\|SUM\\|AVERAGE\\|FROM_UNIXTIME\\|NOT\\|ISNULL"
                 0 font-lock-type-face t)))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(partial\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "Þ")
                               nil)))
                    ("(\\(comp\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "º")
                               nil)))
                    )))


(require 'clj-refactor)

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "s-r")))

(global-set-key (kbd "<M-s-right>") 'buf-move-right)

;;(yas-global-mode 1)

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (projectile-mode . " P")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (clojure-mode . "Clj")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx")
    (smart-tab-mode . "")
    (highlight-parentheses-mode . "")
    (auto-fill-function " ι")
    (clj-refactor-mode " ζ")
    (cider-mode " Ξ")
    (elisp-slime-nav-mode . " sN")
    )
  "Alist for `clean-mode-line'.
 
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
 
 
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
 
 
(add-hook 'after-change-major-mode-hook 'clean-mode-line)
(which-func-mode 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
;;(setq web-mode-engines-alternate-delimiters '(("smarty" . ("{\?" . "\?}"))))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq js-indent-level 4)

(require 'gnuplot-mode)
(setq gnuplot-program "/usr/local/bin/gnuplot")


(put 'narrow-to-region 'disabled nil)

(global-git-gutter-mode 1)
