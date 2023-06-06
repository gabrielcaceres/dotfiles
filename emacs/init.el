
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC SETUP

;; Package Manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
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
;; (setq use-package-always-ensure t)

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
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (defun track-mouse (e)) 
;;   (setq mouse-sel-mode t)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display config

;; Set default Emacs frame size
(setq default-frame-alist '((width . 94) (height . 50)))

;; Set default font
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))
;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-12" ))
;; (add-to-list 'default-frame-alist '(font . "Anonymous Pro-14" ))
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-16:weight=regular" ))
;; (add-to-list 'default-frame-alist '(font . "Inconsolata-17:weight=regular:width=semicondensed" ))

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

;;* Highlight matching parentheses
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; (show-paren-mode t)

;;* Show line number on left side of window
;; use "display-line-number-mode' instead; see package below
;; (global-display-line-numbers-mode)

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

;; Revert buffers when the underlying file had changed
;; (global-auto-revert-mode 1)

