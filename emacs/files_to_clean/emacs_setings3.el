;; .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI CUSTOMIZATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (company-reftex company-quickhelp flycheck-pos-tip company-c-headers company-bibtex company-auctex company-anaconda anaconda-mode company smart-forward flycheck-color-mode-line r-autoyas auctex rainbow-delimiters))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PERSONAL CUSTOMIZATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC SETUP

;; Package Manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

;; Location to install package
(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))

;; To correctly recognize enviroment variables
;; see: http://zmjones.com/mac-setup/
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL CONFIG

;;* Set default Emacs frame size
(setq default-frame-alist '((width . 94) (height . 50)))
;; Set default font
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))
;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-12" ))
;; (add-to-list 'default-frame-alist '(font . "Anonymous Pro-14" ))
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-18:weight=regular" ))
(add-to-list 'default-frame-alist '(font . "Inconsolata-18" ))
;; Increase line spacing
(setq-default line-spacing 2)
;;* Show line number in the mode line
;; (setq line-number-mode t)
;;* Show column number in the mode line
(setq column-number-mode t)
;; ;;* Focus follows mouse
;; (setq mouse-autoselect-window t)
;;* Change threshold for automatic horizontal split
(setq split-width-threshold 120)
;;* Delete highlighted text when typing
(delete-selection-mode t)
;;* Highlight matching parentheses
(show-paren-mode t)
;;* Show line number on left side of window
;; use "display-line-number-mode' instead
;; (global-linum-mode t)
;; Disable toolbar
(tool-bar-mode -1)
;; Hide welcome screen
(setq inhibit-startup-screen t)
;; Use "command" key to move windows in frame
(windmove-default-keybindings 'super)
;; ;; Make shell mode (e.g. iESS) not editable ;(*** doesn't work?)
;; (setq comint-prompt-read-only t)

;; Iterm2 emacs mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t)
  )

;; ;; Easier access to past commands
;; ;; https://ess.r-project.org/Manual/ess.html#Command-History
;; (eval-after-load "comint"
;;   '(progn
;;      (define-key comint-mode-map [up]	;up arrow for previous input
;;        'comint-previous-matching-input-from-input)
;;      (define-key comint-mode-map [down]	;down arrow for next input
;;        'comint-next-matching-input-from-input)

;;      ;; also recommended for ESS use --
;;      (setq comint-move-point-for-output 'others) ;moves point to prompt when sending output
;;      ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
;;      (setq comint-scroll-to-bottom-on-input 'this)
;;      ))

;;;;;;;;;;;;;****************

;; (load "ess-autoloads")
(use-package ess-r-mode
  ;; :disabled)
  :bind
  ("M-=" . ess-insert-assign)
  ;;* Make shell read-only
  ;; (setq comint-prompt-read-only t)
  :config
  (setq ess-default-style 'RRR)  
  (ess-toggle-underscore nil)	  ;disable underscore assign
  (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)
  (define-key ess-mode-map [(kp-enter)] 'ess-eval-region-or-line-and-step)
  ;;* Smart comma
  (setq ess-R-smart-operators t)
  ;;* Don't ask for working directory; use current
  (setq ess-ask-for-ess-directory nil)
  ;;* Name *R* interactive buffer
  ;; (setq ess-local-process-name "R")
  ;;* Create a new frame for each help instance
  ;; (setq ess-help-own-frame t)
  ;;* All help buffers go into single new frame
  ;; (setq ess-help-own-frame 'one)
  ;;* Remove trailing whitespace after each line
  ;; (setq ess-nuke-trailing-whitespace-p 'ask) ;ask for confirmation
  (setq ess-nuke-trailing-whitespace-p t) ;automatic
  (setq inferior-R-args "--no-save --no-restore")
  )

(use-package rainbow-delimiters-mode
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; :disabled
  :hook (prog-mode inferior-ess-mode)
  )
(use-package smartparens-mode
  ;; https://media.readthedocs.org/pdf/smartparens/latest/smartparens.pdf
  ;; :disabled
  :hook (prog-mode inferior-ess-mode)
  ;; :init
  ;; (sp-local-pair 'ess-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  ;; (defun my-create-newline-and-enter-sexp (&rest _ignored)
  ;;   "Open a new brace or bracket expression, with relevant newlines and indent. "
  ;; (newline)
  ;; (indent-according-to-mode)
  ;; (forward-line -1)
  ;; (indent-according-to-mode))
  )


(use-package company
  ;; :disabled
  ;; :bind
  ;; ("TAB" . company-indent-or-complete-common)
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.3;1.5
	company-minimum-prefix-length 2
	;; company-auto-complete t
	)
  )


(use-package company-reftex
  :disabled)
(use-package company-quickhelp
  :disabled)
(use-package flycheck-pos-tip
  :disabled)
(use-package company-c-headers
  :disabled)
(use-package company-bibtex
  :disabled)
(use-package company-auctex
  :disabled)
(use-package company-anaconda
  :disabled)
(use-package anaconda-mode
  :disabled)
(use-package poly-R
  :disabled)
(use-package ein
  :disabled)
(use-package smart-forward
  :disabled)
(use-package flycheck-inline
  :disabled)
(use-package flycheck-color-mode-line
  :disabled)
(use-package flycheck
  :disabled)
(use-package electric-operator
  :disabled)
(use-package yasnippet-snippets
  :disabled)
(use-package yasnippet
  :disabled)
(use-package r-autoyas
  :disabled)
(use-package auctex
  :disabled)
(use-package use-package
  :disabled)

                       
