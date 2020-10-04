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
;; prefixes
(global-set-key (kbd "C-b") ctl-x-map) ;; (buffer-mode) = C-b
(keyboard-translate ?\C-c ?\C-u) ;; translates (user-mode) = C-u
(keyboard-translate ?\C-u ?\C-c) ;; translates (copy C-u) = C-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; user-mode
(global-set-key (kbd "C-c t") 'toggle-frame-split)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions
(defun test-command ()
  "A String representing the test command to run for the given context."
  (cond
   ((eq major-mode 'java-mode) "mvn test")
   ((eq major-mode 'c-mode) "make test")))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
