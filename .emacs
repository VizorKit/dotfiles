;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use-package
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
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq explicit-bash.exe-args '("--login" "-i"))
  (setq default-directory "~/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic emacs setup
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-display-line-numbers-mode)
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
;; prefixes
(global-set-key (kbd "C-b") ctl-x-map) ;; (buffer-mode) = C-b
(keyboard-translate ?\C-c ?\C-u) ;; translates (user-mode) = C-u
(keyboard-translate ?\C-u ?\C-c) ;; translates (copy C-u) = C-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom functions binding
(global-set-key (kbd "C-c t") 'toggle-frame-split)
(global-set-key (kbd "C-c i") 'delete-between-pair)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages
;;; low contrast theme
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-override-colors-alist
	'(("zenburn-red" . "#de7a78")
	  ("zenburn-bg-05" . "#292928")
	  ("zenburn-bg" . "#3b3837"))))

;;; high contrast custom theme
(use-package autothemer
  :ensure t
  :init
  (autothemer-deftheme
   vizorkit "A theme."
   ((((class color) (min-colors #xFFFFFF)))
    (vk-cyan "cyan")
    (vk-black "black")
    (vk-lt-green "light green")
    (vk-yellow "yellow")
    (vk-purple "magenta")
    (vk-white "ghost white")
    (vk-green "green")
    (vk-gray "gray50"))
   ((default (:foreground vk-white :background vk-black))
    (font-lock-keyword-face (:foreground vk-yellow :weight 'bold))
    (font-lock-constant-face (:foreground vk-green :weight 'bold))
    (font-lock-comment-face (:foreground vk-cyan))
    (font-lock-string-face (:foreground vk-purple))
    (font-lock-builtin-face (:foreground vk-lt-green :slant 'italic))
    (font-lock-function-name-face (:foreground vk-green :slant 'italic))
    (font-lock-variable-name-face (:foreground vk-lt-green :weight 'bold))
    (region (:background vk-gray))
    (cursor (:background vk-yellow))))
  :config
  (enable-theme 'vizorkit))

;;; color identifiers for supported languages
(use-package color-identifiers-mode
  :ensure t)

;;; company
(use-package company
  :ensure t
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

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
  (global-set-key (kbd "C-c p a") 'counsel-ag)
  (global-set-key (kbd "M-x") 'counsel-M-x))

;;; projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/source/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ide features

;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-idle-delay .01
	lsp-signature-doc-lines 5)
  (setq lsp-keymap-prefix "C-l")
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (lsp-enable-which-key-integration t))

;;; typescript mode
(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'html-mode-hook 'lsp)
  (setq typescript-indent-level 2))

;;; lsp ui
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-l c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (global-flycheck-mode))

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
  "Seek backwards to a character"
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
  (let ((result ()))
    (dolist (x char-pairs)
      (setq start (car x))
      (setq end (cdr x))
      (when (or (= chr start) (= chr end))
	(setq result x)))
    result))

(defun get-start-char (chr)
  (car (get-char-pair chr)))
(defun get-end-char (chr)
  (cdr (get-char-pair chr)))

(defun seek-to-matching-char (start end count)
  (while (> count 0)
    (if (= (following-char) end)
	(setq count (- count 1))
      (if (= (following-char) start)
	  (setq count (+ count 1))))
    (forward-char 1)))

(defun seek-backward-to-matching-char (start end count)
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
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char 1)
  (setq mark (point))
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char -1)
  (kill-region mark (point)))

(defun delete-all-pair (char)
  "Delete in between the given pair and the characters"
  (interactive "cDelete all char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (setq mark (point))
  (forward-char 1)
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (kill-region mark (point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
