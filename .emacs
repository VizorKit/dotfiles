;; Do not show the startup screen and disable bells
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode t)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package company
  :ensure t
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions
;; opens a shell in other window
(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vars
;; my minor mode
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t t") 'shell-other-window)
    map))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks
(add-hook 'after-init-hook 'global-company-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys

;; do the installation
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit use-package neotree lsp-ui lsp-ivy helm-lsp flycheck evil dracula-theme dap-mode company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
