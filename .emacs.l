;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-time settings
(setq gc-cons-threshold 500000000)
(setq gc-cons-percentage .8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; windows setup
(when (eq system-type 'windows-nt)
  ;; be sure to set the HOME environment variable and install git bash
  (setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
  (setq explicit-bash.exe-args '("--login" "-i"))
  (setq default-directory "~/"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic emacs setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)
(setq  fast-but-imprecise-scrolling nil jit-lock-defer-time 0)
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(electric-pair-local-mode 1)
(electric-pair-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-display-line-numbers-mode)
(setq byte-compile-warnings '(cl-functions))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic editor
(global-set-key (kbd "C-z") 'undo) ;; undo
(global-set-key (kbd "C-s") 'save-buffer) ;; save file
(global-set-key (kbd "C-x") 'kill-region) ;; kills marked region. last marked region if none set.
(global-set-key (kbd "C-w") 'write-file) ;; save as
(global-set-key (kbd "C-v") 'yank) ;; paste
(global-set-key (kbd "C-u") 'kill-ring-save) ;; copy (will be translated to C-c)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal) ;; quit
(global-set-key (kbd "C-r") 'backward-char) ;; r for reverse f for forward
(global-set-key (kbd "C-t") 'kill-word-at-point) ;; terminate word at point.
(global-set-key (kbd "C-;") 'kill-line-at-point) ;; kills statements like c# ; (line get it?)
;; prefixes
(global-set-key (kbd "C-b") ctl-x-map) ;; (buffer-mode) = C-b
(keyboard-translate ?\C-c ?\C-u) ;; translates (user-mode) = C-u
(keyboard-translate ?\C-u ?\C-c) ;; translates (copy C-u) = C-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom functions binding
(global-set-key (kbd "C-c t") 'toggle-frame-split)
(global-set-key (kbd "C-c i") 'delete-between-pair)
(global-set-key (kbd "C-c k") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c .") 'query-replace-from-top)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages
;;; soft contrast custom theme
(use-package autothemer
  :ensure t
  :init
  (autothemer-deftheme
   chris-theme "A theme."
   ((((class color) (min-colors #xFFFFFF)))
    (vk-blendin "gray45")
    (vk-constant "dark green")
    (vk-easy-text "black")
    (vk-standout "DeepPink4")
    (vk-golden "chocolate")
    (vk-literal "RoyalBlue3")
    (vk-background "gray80")
    (vk-visible "peach puff")
    (vk-funcs "dark green")
    (vk-err "red")
    (vk-highlight "antique white"))
   ((default (:foreground vk-easy-text :weight 'bold :background vk-background :font "Consolas" :height 120))
    (font-lock-keyword-face (:foreground vk-golden))
    (font-lock-constant-face (:foreground vk-constant :underline t))
    (font-lock-comment-face (:foreground vk-blendin))
    (font-lock-string-face (:foreground vk-literal))
    (font-lock-builtin-face (:foreground vk-standout :slant 'italic))
    (font-lock-function-name-face (:foreground vk-funcs :slant 'italic))
    (font-lock-variable-name-face (:foreground vk-easy-text))
    (font-lock-type-face (:foreground vk-standout))
    (error (:foreground vk-err))
    (warning (:foreground vk-golden))
    (company-tooltip (:background vk-highlight))
    (company-tooltip-common (:foreground vk-literal))
    (hl-line (:background vk-highlight))
    (region (:background vk-visible ))
    (cursor (:background vk-golden))))
  :config
  (enable-theme 'chris-theme))
;;; company
(use-package company
  :ensure t
  :hook ((after-init . global-company-mode))
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-require-match nil))
;;; which-key
(use-package which-key
  :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode))
;;; ivy
(use-package ivy
  :ensure t
  :config
  (global-set-key (kbd "C-b b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-b C-b") 'ivy-switch-buffer-other-window)
  (setq ivy-use-virtual-buffers t
                ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))
;;; counsel
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-b s") 'swiper-isearch)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-b f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x))
;;; projectile
(use-package projectile
  :ensure t
  :hook ((after-init . projectile-mode))
  :config
  (setq projectile-project-search-path '("~/source/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package color-identifiers-mode
  :ensure t
  :config
  (setq color-identifiers:timer (run-with-idle-timer 2 t 'color-identifiers:refresh))
  (add-hook 'c-mode-hook 'color-identifiers-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ide features
;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((c-mode . lsp)
	 (c++-mode . lsp))
  :init
  (setq lsp-idle-delay .01
                lsp-signature-doc-lines 5)
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-log=error"))
  (add-hook 'after-init-hook 'global-company-mode)
  (lsp-enable-which-key-integration t))
;;; lsp-ivy
(use-package lsp-ivy
  :after ivy
  :defer t
  :ensure t)
;;; flychceck
(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-l c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (global-flycheck-mode))
;;; tabout!
(use-package tab-jump-out
  :ensure t
  :config
  (define-globalized-minor-mode my-global-tabout-mode tab-jump-out-mode
  (lambda () (tab-jump-out-mode 1)))
  (my-global-tabout-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom functions
;;; toggle frame split
(defun toggle-frame-split ()
    "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
    (interactive)
    (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
    (let ((split-vertically-p (window-combined-p)))
      (delete-window)
      (if split-vertically-p
                  (split-window-horizontally)
                (split-window-vertically))
      (switch-to-buffer nil)))
;;; delete inbetween
(defun seek-backward-to-char (chr)
  "Seek backwards to a CHR."
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))
(setq char-pairs
      '(( ?/  . ?/  )
                ( ?\" . ?\" )
                ( ?\' . ?\' )
                ( ?\( . ?\) )
                ( ?\[ . ?\] )
                ( ?\{ . ?\} )
                ( ?<  . ?>  )))
(defun get-char-pair (chr)
  "Get the pair of CHR from list."
  (let ((result ()))
    (dolist (x char-pairs)
      (setq start (car x))
      (setq end (cdr x))
      (when (or (= chr start) (= chr end))
                (setq result x)))
    result))
(defun get-start-char (chr)
  "Get the start CHR."
  (car (get-char-pair chr)))
(defun get-end-char (chr)
  "Get the end CHR."
  (cdr (get-char-pair chr)))
(defun seek-to-matching-char (start end count)
  "START char to END char, return COUNT."
  (while (> count 0)
    (if (= (following-char) end)
                (setq count (- count 1))
      (if (= (following-char) start)
                  (setq count (+ count 1))))
    (forward-char 1)))
(defun seek-backward-to-matching-char (start end count)
  "START char to END char, return COUNT."
  (if (= (following-char) end)
      (forward-char -1))
  (while (> count 0)
    (if (= (following-char) start)
                (setq count (- count 1))
      (if (= (following-char) end)
                  (setq count (+ count 1))))
    (if (> count 0)
                (forward-char -1))))
(defun delete-between-pair (char)
  "Delete in between the given pair of CHAR."
  (interactive "cDelete between char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char 1)
  (setq mark (point))
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char -1)
  (kill-region mark (point)))
(defun delete-all-pair (char)
  "Delete in between the given pair and the CHAR."
  (interactive "cDelete all CHAR: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (setq mark (point))
  (forward-char 1)
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (kill-region mark (point)))
(defun kill-thing-at-point (thing)
  "Kill the THING at point."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))
(defun kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (kill-thing-at-point 'word))
(defun kill-line-at-point ()
  "Kill the word at point."
  (interactive)
  (kill-thing-at-point 'line))
(defun query-replace-from-top ()
  "Move to top of buffer to search."
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (goto-char (point-min))
      (call-interactively 'query-replace))
    (message "Back to old point.")
    (goto-char orig-point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; profiling & normal gc settings
(setq gc-cons-threshold 80000)
(setq gc-cons-percentage .1)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end for custom-set variables

