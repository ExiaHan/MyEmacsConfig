;; Disable bidirectrional edting for just speed up emacs when processing large single line
(setq-default bidi-paragraph-direction 'left-to-right)
(global-so-long-mode 1)
;; Disable line wrap by default to avoid long lines are wraped into too many shortlines which affect the format indent
(set-default 'truncate-lines t)
;; Set tab to space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set [Non]GNU [M]ELPA to use USTC mirror
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
;; Global Line Num Mode
(if (>= emacs-major-version 26)
    (progn (global-display-line-numbers-mode 1)
	   (global-hl-line-mode 1)
	   (set-face-foreground 'line-number "#9999ff"))
  (progn (require 'linum)
	 (global-linum-mode t)
	 (setq linum-format "%4d| ")
	 (set-face-foreground 'linum "orange")
	 (add-to-list 'load-path "~/.emacs.d/plugins/hlinum-mode")
	 (require 'hlinum)
	 (hlinum-activate)))

;; Dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/dracula/")
(load-theme 'dracula t)

;; 80 Ruler
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)

;; Toggle C++ comments "//" in C-mode
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Transparent Emacs C-t t
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set background transparent
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Make searches case insensitive
(setq case-fold-search t)
(setq completion-ignore-case  t)

;; Make file name completion case insensitive
(setq read-file-name-completion-ignore-case t)

;; Make buffer name completion case insensitive
(setq read-buffer-completion-ignore-case t)

;; Powerline and Smart-mode-line
(add-to-list 'load-path "~/.emacs.d/plugins/powerline/")
(add-to-list 'load-path "~/.emacs.d/plugins/rich-minority")
(add-to-list 'load-path "~/.emacs.d/plugins/smart-mode-line/themes/")
(add-to-list 'load-path "~/.emacs.d/plugins/smart-mode-line/")

;; If you want Powerline, uncommented below
(require 'powerline)
(powerline-default-theme)

;; If you want Smart mode line, uncomment below
;(load-file "~/.emacs.d/plugins/smart-mode-line/themes/smart-mode-line-light-powerline-theme.el")
;(require 'smart-mode-line)
;(require 'powerline)
;(setq sml/theme 'light-powerline)
;(setq sml/no-confirm-load-theme t)
;(sml/setup)
;; Below is auto generated if you choosed to use smart-mode-line
;; It will be generated if you type M-X: sml/setup

;; company-mode, an auto-complete plugins
(add-to-list 'load-path "~/.emacs.d/plugins/company-mode/")
(load-file "~/.emacs.d/plugins/company-mode/company.el")
;(global-company-mode t)
;(require 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
;; smali support
(add-to-list 'load-path "~/.emacs.d/plugins/Emacs-Smali")
; load the smali/baksmali mode
(autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali issues" t)
(add-to-list 'auto-mode-alist '(".smali$" . smali-mode))

;; Python Mode
;(autoload 'python-mode "python-mode.el" "Python mode." t)
;(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))

;; MarkDown Support
(add-to-list 'load-path "~/.emacs.d/plugins/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode) t)

;; MarkDown Preview Support
(add-to-list 'load-path "~/.emacs.d/plugins/markdown-preview-mode")

;; xcscope configure
(add-to-list 'load-path "~/.emacs.d/plugins/xcscope.el")
(require 'xcscope)
(cscope-setup)

;; Highlight line number
;; (add-to-list 'load-path "~/.emacs.d/plugins/hlinum-mode")
;; (require 'hlinum)
;; (hlinum-activate)

;; Kotlin mode
(add-to-list 'load-path "~/.emacs.d/plugins/kotlin-mode")
(autoload 'kotlin-mode "kotlin-mode" "Major mode for editing kotlin files" t)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode) t)

;; Protobuf mode
(add-to-list 'load-path "~/.emacs.d/plugins/protobuf")
(autoload 'protobuf-mode "protobuf-mode" "Major mode for editing proto files" t)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode) t)

;; Indent-line
(add-to-list 'load-path "~/.emacs.d/plugins/indent-guide")
(require 'indent-guide)
(indent-guide-global-mode t)

;; AUCTeX mode
;; Check whether is GNU/Linux
(cond ((eq system-type 'gnu/linux)
    (require 'tex-site)
    (load "auctex.el" nil t t)
    (require 'tex-mik)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (setq TeX-PDF-mode t)
    (require 'tex)
    (TeX-global-PDF-mode t)
))
;; View Large File
(add-to-list 'load-path "~/.emacs.d/plugins/vlfi")
(require 'vlf-setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
