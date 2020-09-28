;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic emacs
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages
;; basic theme
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
  (setq company-idle-delay 0.2)
  (global-company-mode t))

;; shows completion key options
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; git config
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; basic help
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable))

;; basic search
(use-package ivy
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (setq ivy-use-virtual-buffers t
                ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
                '((ivy-switch-buffer . ivy--regex-plus)
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

;; advanced project maneuvering
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begin ide
(use-package lsp-mode
  :ensure t
  :hook (
                (java-mode . lsp)
		(c-mode . lsp)
                (lsp-mode . (lambda ()
                                       (let ((lsp-keymap-prefix "C-l"))
                                                (lsp-enable-which-key-integration)))))
  :init
  (setq lsp-idle-delay .02
                lsp-signature-doc-lines 5)
  :config
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  :commands lsp)

;; completes most with lsp
(use-package company-lsp
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; peek capabilities
(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map (kbd "C-l .") 'xref-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-l /") 'xref-find-references)
  :commands lsp-ui-mode)

;; search capabilities
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; todo:: configure
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; debug mode still need to configure
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; java configuration, set lombok path
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

;; does some basic error checking/linting
(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-l c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (global-flycheck-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/snippets-collection"))
  (yas-global-mode 1))
;; todo:: configure
(use-package hydra
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions
(defun test-command ()
  "A String representing the test command to run for the given context."
  (cond
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set keys
(global-set-key (kbd "C-c k") 'delete-other-windows)
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-c 0") 'delete-window)
(global-set-key (kbd "C-c t") 'toggle-frame-split)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'java-mode-hook #'lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sets
(setq c-default-style
      '((c-mode . "linux")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
