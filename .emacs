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
(global-set-key (kbd "C-c") 'kill-ring-save) ;; copy
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal) ;; quit
(global-set-key (kbd "C-r") 'backward-char) ;; r for reverse f for forward
;; prefixes
(global-set-key (kbd "C-b") ctl-x-map) ;; (buffer-mode) = C-b
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") mode-specific-map) ;; (user-mode) = C-u
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
  (setq company-idle-delay 0.2)
  (global-company-mode t))

;; shows completion key options
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
          (t . ivy--regex-fuzzy))))

;; basic project maneuvering
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/source/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-u p") 'projectile-command-map)
  (projectile-mode))

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

;; snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/snippets-collection"))
  (global-set-key (kbd "C-u") yas-minor-mode-map)
  (yas-global-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ide
