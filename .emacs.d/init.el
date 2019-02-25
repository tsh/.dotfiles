(package-initialize)

(package-refresh-contents)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar package-list
  '(
    evil
    darcula-theme
    ))


(tool-bar-mode -1)
(toggle-scroll-bar -1) 
(menu-bar-mode -1) 

(set-frame-font "Source Code Pro-16")
(global-visual-line-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(global-linum-mode t)
(setq inhibit-startup-screen t)

(ido-mode 1)
(ido-everywhere 1)


;; lock file, auto-save and auto-backup
(setq backup-directory-alist
     `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(load-theme 'darcula t)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq tramp-default-method "ssh")
