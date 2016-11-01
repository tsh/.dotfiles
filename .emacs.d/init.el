(require 'package)

(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

;;(unless package-archive-contents
  ;;(package-refresh-contents))

(package-initialize)
(package-refresh-contents)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar package-list
  '(smex
    neotree
    anaconda-mode
    zenburn-theme
    ))

(dolist (p package-list)
  (when (not (package-installed-p p))
         (package-install p)))

(tool-bar-mode -1)
(set-frame-font "Source Code Pro")
(global-visual-line-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(global-linum-mode t)

(ido-mode 1)
(ido-everywhere 1)


;; NEOTREE
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; PYTHON
(add-hook 'python-mode-hook 'anaconda-mode)


;; THEMES
;;(load-theme 'monokai t)
(load-theme 'zenburn t)
