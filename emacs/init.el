
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC SETUP

;; Package Manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

;; Location to install packages
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Change location of custom file (from UI)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ;; To install any missing packages 
(setq use-package-always-ensure t)

;; ;; Automatically update packages using use-package
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAC SPECIFIC SETTINGS

;; To correctly recognize enviroment variables in macos
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Set Mac-specific options
(when (eq system-type 'darwin)
  (setq ;mac-option-key-is-meta nil
	;mac-command-key-is-meta t
	mac-command-modifier 'super
	mac-option-modifier 'meta)
  ;; combined with emacs-mac this gives good pdf quality for retina display
  (setq pdf-view-use-scaling t)
  ;; Set usual command-key shortcuts
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-z") 'undo)
  )

;; ***TODO: I think these should go in the conditional statement above
;; specific to mac
;; Titlebar theme (add-to-list 'default-frame-alist
;; '(ns-transparent-titlebar . t))

;; (add-to-list 'default-frame-alist
;;              '(ns-appearance . dark)) ;; or dark - depending on your theme


;; ;; iterm2 emacs mouse support
;; https://www.emacswiki.org/emacs/iTerm2#h5o-6
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (defun track-mouse (e)) 
;;   (setq mouse-sel-mode t)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display config

(require 'color)

;; Set default Emacs frame size
(setq default-frame-alist '((width . 105) (height . 50)))

;; Set default font
;; (add-to-list 'default-frame-alist '(font . "Monospace-12"))
;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-12" ))
;; (add-to-list 'default-frame-alist '(font . "Anonymous Pro-14" ))
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-16:weight=regular" ))
;; (add-to-list 'default-frame-alist '(font . "Inconsolata-14:weight=regular" ))
;; (add-to-list 'default-frame-alist '(font . "Monaco-14"))
(cond 
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco-14"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-14:weight=regular"))
 ((find-font (font-spec :name "Source Code Pro"))
  (set-frame-font "Source Code Pro-12:weight=regular"))
 ((find-font (font-spec :name "Anonymous Pro"))
  (set-frame-font "Anonymous Pro-14"))
 )

;; Increase line spacing
(setq-default line-spacing 2)

;;* Show line number in the mode line
(setq line-number-mode t)

;;* Show column number in the mode line
(setq column-number-mode t)

;; ;;* Focus follows mouse
;; (setq mouse-autoselect-window t)

;;* Change threshold for automatic horizontal split
;; (setq split-width-threshold 120)
;; (setq split-height-threshold 120)
(setq split-width-threshold 120)

;; Disable toolbar
(tool-bar-mode -1)

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Theme
;; Options I like: "misterioso", "tango-dark"
(load-theme 'misterioso)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing config

;;* Delete highlighted text when typing
(delete-selection-mode t)

;; Is this still necessary?? seems to work without...
;;* Highlight matching parentheses
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; (show-paren-mode t)

;; Use "command key" to move between windows in frame
(windmove-default-keybindings 'super)

;; Automatically copy text selected with the mouse
(setq mouse-drag-copy-region t)
;; Save whatever’s in the current (system) clipboard before
(setq save-interprogram-paste-before-kill t)
;; Mouse side scroll
(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 10)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 10)))

;; Controlling movement of point
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html
;; (setq track-eol t)			; Keep cursor at end of lines.
;; (setq line-move-visual nil)		; Skip logical, rather than visual, lines

(setq-default kill-whole-line t)	; Kill line including '\n'

(setq next-line-add-newlines t)		; down-arrow adds line at eod

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL CONFIG

(use-package tramp
  :ensure nil
  :defer t
  :config
  ;; Get path when ssh-ing
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; ;; Make shell mode (e.g. iESS) not editable ;(*** doesn't work?)
;; (setq comint-prompt-read-only t)

;; Show minibuffer completion options vertically
(setq completions-format 'vertical)

;; Middle button on buffer name shows buffer list
(define-key mode-line-buffer-identification-keymap [mode-line mouse-2] 'buffer-menu)

;; DocView resolution
(setq doc-view-resolution 300)

;; Revert buffers when the underlying file had changed
(global-auto-revert-mode 1)

;;;;;;; Conda settings?? old so not sure if they work
;; (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
;; (setenv "WORKON_HOME" "/ssh:extdatajl:/home/gabrielcaceres/.conda/envs")
;; (setenv "WORKON_HOME" "/home/gabrielcaceres/.conda/envs")
;; ********
;; (setenv "WORKON_HOME" "/usr/local/anaconda3/envs")
;; (pyvenv-mode 1)
;; ;; possible good alias
;; (defalias 'workon 'pyvenv-workon)
;; \********


;;;;;; possible modes to look more into

;; track recent files
;; (recentf-mode 1)

(use-package flyspell
  ;; :disabled
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  )

(use-package hl-line
  ;; :disabled
  :hook (prog-mode . hl-line-mode)
  :custom-face
  ;; (hl-line ((t (:inherit nil :background "#3e4446"))))
  ;; (hl-line ((t (:foreground nil :background (color-darken-name "#3e4446" 4)))))
  (hl-line ((t (:inherit nil :background ,(color-lighten-name (face-attribute 'default :background) 5) ))))
  )

;; Show lines numbers on left side of frame
(use-package display-line-numbers
  ;; :disabled
  ;; :after hl-line
  :hook (prog-mode . display-line-numbers-mode)
  :custom-face                          ;Customize line number colors
  (line-number
   ((t (:inherit shadow
                 :foreground ,(color-darken-name (face-attribute 'shadow :foreground) 22)
                 ;; :background ,(color-darken-name (face-attribute 'default :background) 2)
                 )))) 
  (line-number-current-line
   ((t (:inherit hl-line
                 :foreground ,(color-lighten-name (face-attribute 'shadow :foreground) 2)
                 ;; :background ,(color-lighten-name (face-attribute 'default :background) 4)
                 )))) 
  )

(use-package smartparens
  ;; :init
  ;; (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  ;; (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  ;; (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  ;; (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  ;; (bind-key "C-S-s" #'sp-splice-sexp)
  ;; (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  ;; (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

  :config
  (smartparens-global-mode t)

  (setq sp-highlight-pair-overlay nil)

  ;; (sp-local-pair 'poly-r-sql-mode "/* Start SQL Query */" "/* End SQL Query */"
  ;;       	 :trigger "\\sql")
  )

(use-package rainbow-delimiters-mode
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; :disabled
  :ensure rainbow-delimiters
  :hook (prog-mode inferior-ess-mode)
  :init
  ;; (custom-set-faces
  ;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
  ;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
  ;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
  ;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
  ;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
  ;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
  ;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
  ;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
  ;; rainbow-delimiters
  ;;
  ;; ZENBURN THEME
  ;; (setq rainbow-delimiters-max-face-count 8)
  (let ( (zenburn-fg		"#dcdccc")
       (zenburn-bg-1		"#2b2b2b")
       (zenburn-bg		"#3f3f3f")
       (zenburn-bg+1		"#4f4f4f")
       (zenburn-bg+2		"#5f5f5f")
       (zenburn-red+1		"#dca3a3")
       (zenburn-red		"#cc9393")
       (zenburn-red-1		"#bc8383")
       (zenburn-red-2		"#ac7373")
       (zenburn-red-3		"#9c6363")
       (zenburn-red-4		"#8c5353")
       (zenburn-orange		"#dfaf8f")
       (zenburn-yellow		"#f0dfaf")
       (zenburn-yellow-1	"#e0cf9f")
       (zenburn-yellow-2	"#d0bf8f")
       ;; 
       (zenburn-green-4         "#2e3330")
       (zenburn-green-1		"#5f7f5f")
       (zenburn-green		"#7f9f7f")
       (zenburn-green+1		"#8fb28f")
       (zenburn-green+2		"#9fc59f")
       (zenburn-green+3		"#afd8af")
       (zenburn-green+4		"#bfebbf")
       (zenburn-cyan		"#93e0e3")
       (zenburn-blue+1		"#94bff3")
       (zenburn-blue		"#8cd0d3")
       (zenburn-blue-1		"#7cb8bb")
       (zenburn-blue-2		"#6ca0a3")
       (zenburn-blue-3		"#5c888b")
       (zenburn-blue-4		"#4c7073")
       (zenburn-blue-5		"#366060")
       (zenburn-magenta		"#dc8cc3"))
    ;; 
    ;; (custom-set-faces
    ;;  ;; `(rainbow-delimiters-depth-1-face "orange")
    ;;  `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-cyan))))
    ;;  `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-yellow))))
    ;;  `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-blue+1))))
    ;;  `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-red+1))))
    ;;  `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-green+1))))
    ;;  `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-blue-1))))
    ;;  `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-orange))))
    ;;  `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-magenta))))
    ;;  `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-yellow-2))))
    ;;  `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-green+2))))
    ;;  `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-blue+1))))
    ;;  `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-red-4))))
    ;;  )
    (custom-set-faces
     ;; `(rainbow-delimiters-depth-1-face "orange")
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-blue-1))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-orange))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-green+3))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-magenta))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-blue+1))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-cyan))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-red+1))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-green+4))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-yellow))))
     `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-green+1))))
     `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-green+2))))
     `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-blue+1))))
     )
    )
  )


(cond 
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco-14"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-14:weight=regular"))
 ((find-font (font-spec :name "Source Code Pro"))
  (set-frame-font "Source Code Pro-12:weight=regular"))
 ((find-font (font-spec :name "Anonymous Pro"))
  (set-frame-font "Anonymous Pro-14"))
 )

(use-package conda
  ;; https://github.com/necaris/conda.el
  ;; :disabled
  :config
  (custom-set-variables
   (cond                                ;Search possible conda locations
    ((file-directory-p "~/miniforge3")
     '(conda-anaconda-home "~/miniforge3"))
    ((file-directory-p "~/mambaforge")
     '(conda-anaconda-home "~/mambaforge"))
    ))
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "-i --simple-prompt -c \"%load_ext autoreload \n%autoreload 2\"")
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  ;; ;; if you want to automatically activate a conda environment on the opening of a file:
  ;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                            ;; (conda-env-activate-for-buffer))))
  )

(use-package company
  ;; :disabled
  ;; :bind
  ;; ("TAB" . company-indent-or-complete-common)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3
        ;; company-quickhelp-delay 0.0
        )
  ;; set default `company-backends'
  (setq company-backends
        '((company-files               ; files & directory
           company-keywords            ; keywords
           company-capf                ; completion-at-point-functions
           company-dabbrev-code
           company-etags
           company-dabbrev)))
  ;; (company-show-numbers t)
  ;; (company-tooltip-align-annotations 't)
  (global-company-mode t)
  )

(use-package anaconda-mode
  ;; :disabled
  ;; :bind (("C-c C-x" . next-error))
  :hook (python-mode . anaconda-mode)
  :config
  )

(use-package company-anaconda
  ;; :disabled
  :after (anaconda-mode company)
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  ;; (eval-after-load "company"
  ;;   '(add-to-list 'company-backends '(company-anaconda :with company-capf)))
  )

(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :config
  ;; (setq numpydoc-insertion-style 'yas)
  )

(use-package highlight-indent-guides
  ;; https://github.com/DarthFennec/highlight-indent-guides
  :disabled
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  )

;; Syntax checking
(use-package flymake-diagnostic-at-point
  :disabled
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  ;; (setq flymake-diagnostic-at-point-timer-delay 0.7)
  )

(use-package electric-operator
  ;; https://github.com/davidshepherd7/electric-operator
  :disabled
  :hook (ess-r-mode. electric-operator-mode)
  )
