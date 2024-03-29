(require 'package)
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/"))
 ;;(unless package-archive-contents  (package-refresh-contents))


(use-package lsp-mode
  :hook
  ((python-mode . lsp-deffered)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-pylsp
  :ensure t)

;; Load a custom theme
;;(load-theme 'wombat t)
(load-theme 'whiteboard t)
;;; Vim Emulation
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

(unless (package-installed-p 'neotree)
  (package-install 'neotree))

(tool-bar-mode -1)
(toggle-scroll-bar -1) 
(menu-bar-mode -1) 
(set-frame-font "Source Code Pro-14")
(global-visual-line-mode t)
(delete-selection-mode t)
(show-paren-mode t)
;(global-linum-mode t)
(global-display-line-numbers-mode 1)
(save-place-mode 1)

(ido-mode 1)
(ido-everywhere 1)

(setq inhibit-startup-screen t)
;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'arrow)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; lock file, auto-save and auto-backup
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil)
