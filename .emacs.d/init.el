
(require 'package)
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/"))
;;  (unless package-archive-contents  (package-refresh-contents))

(package-initialize)

;; AUTOCOMPLETE
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  )

(add-hook 'after-init-hook 'global-company-mode)

;; company-docs
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


;; LSP
(add-hook 'python-mode-hook 'eglot-ensure)


;; DAP
(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
   (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
   (dape-breakpoint-global-mode)

  ;; Info buffers to the right
   (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
   (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
   (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
   (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))



;; Virtual env
(use-package pyvenv
  :config
  (pyvenv-mode 1))



;; Vim Emulation
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


;; Load a custom theme
;;(load-theme 'wombat t)
 (load-theme 'whiteboard t)


