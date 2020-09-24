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
  (projectile-mode))

;; advanced project maneuvering
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begin ide

;; todo:: configure
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; todo:: configure
(use-package flx
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; todo:: configure
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package dap-mode
  :ensure t)

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'lsp-java-boot-lens-mode)
 (setq lsp-java-vmargs
            `("-noverify"
             "-Xmx1G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              ,(concat "-javaagent:" "~/lombok/lombok.jar")
              ,(concat "-Xbootclasspath/a:" "~/lombok/lombok.jar"))))

;; todo:: configure
(use-package flycheck
  :ensure t)

;; todo:: configure
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set keys
(global-set-key (kbd "C-c k") 'delete-other-windows)
(global-set-key (kbd "C-c o") 'other-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet flycheck lsp-java dap-mode lsp-ui flx lsp-treemacs counsel-projectile projectile counsel magit which-key company zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
