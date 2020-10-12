;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; standard emacs
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
(global-set-key (kbd "<backtab>") 'unindent) ;; unindents
;; prefixes
(global-set-key (kbd "C-b") ctl-x-map) ;; (buffer-mode) = C-b
(keyboard-translate ?\C-c ?\C-u) ;; translates (user-mode) = C-u
(keyboard-translate ?\C-u ?\C-c) ;; translates (copy C-u) = C-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; user-mode
(global-set-key (kbd "C-c t") 'toggle-frame-split)
(global-set-key (kbd "C-c i") 'delete-between-pair)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages
;; low contrast theme
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-override-colors-alist
	'(("zenburn-red" . "#de7a78")
	  ("zenburn-bg-05" . "#292928")
	  ("zenburn-bg" . "#3b3837")))
  (load-theme 'zenburn t))

;; completes most things
(use-package company
  :ensure t
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
  (global-company-mode t))

;; shows completion key options
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; git integration
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;; basic help
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-b s") 'swiper-isearch)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-b f") 'counsel-find-file)
  (global-set-key (kbd "C-c p a") 'counsel-ag)
  (global-set-key (kbd "M-x") 'counsel-M-x))

;; basic search
(use-package ivy
  :ensure t
  :config
  (global-set-key (kbd "C-b b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-b C-b") 'ivy-switch-buffer-other-window)
  (setq ivy-use-virtual-buffers t
  	          ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
	  (swiper-isearch . ivy--regex-plus)
	  (counsel-describe-function . ivy--regex-plus)
	  (counsel-describe-variable . ivy--regex-plus)
	  (counsel-M-x . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

;; basic project maneuvering
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/source/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-test-cmd #'test-command)
  (projectile-mode))

;; counsel on top of projectile
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begin ide
;; enables lsp
(use-package lsp-mode
  :ensure t
  :hook (
          (java-mode . lsp)
          (lsp-mode . (lambda ()
                        (let ((lsp-keymap-prefix "C-l"))
                              (lsp-enable-which-key-integration)))))
  :init
  (setq lsp-idle-delay .02
        lsp-signature-doc-lines 5)
  :config
  (setq c-default-style '((c-mode . "linux")))
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  :commands lsp)

;; completes most lsp
(use-package company-lsp
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; ui ide features
(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map (kbd "C-l .") 'xref-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-l /") 'xref-find-references)
  :commands lsp-ui-mode)

;; does error checking/linting
(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-l c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (global-flycheck-mode))

;; enables typescript mode.
(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'html-mode-hook 'lsp))

;;; be sure to override the lombok path
(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs
            `("-noverify"
              "-Xmx1G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              ,(concat "-javaagent:" "$$LOMBOK_PATH")
              ,(concat "-Xbootclasspath/a:" "$$LOMBOK_PATH"))))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (setq gud-key-prefix (kbd "C-b C-a"))
  (dap-auto-configure-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/snippets-collection"))
  (yas-global-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions
;; add your test commands here
(defun test-command ()
  "A String representing the test command to run for the given context."
  (cond
   ((eq major-mode 'java-mode) "mvn test")
   ((eq major-mode 'c-mode) "make test")))

;; toggles between horizontally and vertically aligned windows.
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

;; unindents
(defun unindent ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
	(untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
	        (replace-match "")))))

;; delete inbetween
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
