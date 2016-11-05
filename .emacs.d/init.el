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
    ace-jump-mode
    company
    company-anaconda
    anaconda-mode
    company-quickhelp
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


;; THEMES
;;(load-theme 'monokai t)
(load-theme 'zenburn t)


;; NEOTREE
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)
;; COMPANY ANACONDA
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
;; COMPANY QUICKHELP
(company-quickhelp-mode 1)
(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))


;; PYTHON
(add-hook 'python-mode-hook 'anaconda-mode)


;; ACE JUMP
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-;") 'ace-jump-mode)
;; enable a more powerful jump back function from ace jump mode
(autoload 
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-jump-mode zenburn-theme company-anaconda company neotree smex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
