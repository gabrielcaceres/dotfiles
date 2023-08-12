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
    (pyvenv poly-R flycheck yasnippet bind-key ess exec-path-from-shell use-package smartparens smartparens-mode rainbow-delimiters-mode ess-r-mode eimp company-reftex company-quickhelp flycheck-pos-tip company-c-headers company-bibtex company-auctex company-anaconda anaconda-mode company smart-forward flycheck-color-mode-line r-autoyas auctex rainbow-delimiters))))
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
;; (require 'exec-path-from-shell)
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))
(if (not (equal window-system 'w32))
    (progn
      (use-package exec-path-from-shell)
      (exec-path-from-shell-initialize)))

;; To install any missing packages
(setq use-package-always-ensure t)

;; Automatically update packages using use-package
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL CONFIG

;; Titlebar theme
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark)) ;; or dark - depending on your theme

;; Get path when ssh-ing
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
(show-paren-mode t)
;;* Show line number on left side of window
;; use "display-line-number-mode' instead
;; (global-display-line-numbers-mode)
;; Disable toolbar
(tool-bar-mode -1)
;; Hide welcome screen
(setq inhibit-startup-screen t)
;; Use "command key" to move between windows in frame
(windmove-default-keybindings 'super)
;; ;; Make shell mode (e.g. iESS) not editable ;(*** doesn't work?)
;; (setq comint-prompt-read-only t)
;; Show minibuffer completion options vertically
(setq completions-format 'vertical)
;; Middle button on buffer name shows buffer list
(define-key mode-line-buffer-identification-keymap [mode-line mouse-2] 'buffer-menu)
;; DocView resolution
(setq doc-view-resolution 300)
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

;; iterm2 emacs mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t)
  )

;; (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
;; (setenv "WORKON_HOME" "/ssh:extdatajl:/home/gabrielcaceres/.conda/envs")
;; (setenv "WORKON_HOME" "/home/gabrielcaceres/.conda/envs")
(setenv "WORKON_HOME" "/usr/local/anaconda3/envs")
(pyvenv-mode 1)
;; possible good alias
(defalias 'workon 'pyvenv-workon)

(require 'color)

;; Customize popup tip color
(let ((fg (face-attribute 'default :foreground)))
  (custom-set-faces
   ;; `(popup-tip-face ((t (:foreground "tomato1" :background ,(color-lighten-name bg 10)))))
   `(popup-tip-face ((t (:background "tomato2" :foreground "white"))))
   )
  )

;; (let ((bg (face-attribute 'default :background))
;;       (fg (face-attribute 'default :foreground)))
;;   (setq pos-tip-foreground-color "white"
;; 	pos-tip-background-color "gray" ;,(color-lighten-name bg 15)
;; 	)
;;   )

;; Customize line number colors
(let ((bg (face-attribute 'default :background))
      (fg (face-attribute 'shadow :foreground)))
  (custom-set-faces
   ;; `(line-number ((t (:background "tomato2" :foreground "white"))))
   ;; `(line-number-current-line ((t (:background "tomato2" :foreground "white"))))
   `(line-number
     ((t (:inherit shadow :foreground ,(color-darken-name fg 18)))))
   `(line-number-current-line
     ((t (:inherit line-number :foreground ,(color-lighten-name fg 10) :background ,(color-darken-name bg 5)))))
     ;; ((t (:inherit line-number :foreground ,(color-lighten-name fg 10)))))
   )
  )

;; Turn on superword mode (e.g. considers snake_case a single word)
;; (add-hook 'prog-mode-hook #'superword-mode)

;; https://www.emacswiki.org/emacs/ESSSpeedbar
;; (speedbar-add-supported-extension ".R")

;;;;;;;;;;;;;****************

;; Profile emacs startup
(use-package esup
  ;; https://github.com/jschaf/esup
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  :commands (esup)
  :config
  (setq esup-depth 0)	     ;https://github.com/jschaf/esup/issues/54
  )

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  )


;; (defun myindent-ess-hook ()
;;   (setq-local ess-style 'OWN)
;;   (setq-local ess-indent-offset 2)
;;   (setq ess-offset-arguments 'open-delim)
;;   (setq ess-offset-arguments-newline 'prev-call)
;;   (setq ess-offset-block 'open-delim)
;;   (setq ess-offset-continued '(straight 2))
;;   (setq ess-align-nested-calls nil)
;;   ;; ess-align-arguments-in-calls '("function[ 	]*(")
;;   (setq ess-align-continuations-in-calls nil)
;;   (setq ess-align-blocks '(control-flow))
;;   (setq ess-indent-from-lhs '(arguments fun-decl-opening))
;;   (setq ess-indent-from-chain-start nil)
;;   (setq ess-indent-with-fancy-comments t)
;;   )
;; (add-hook 'ess-r-mode-hook 'myindent-ess-hook)

;; (load "ess-autoloads")
(use-package ess-r-mode
  ;; :disabled)
  :defer t
  :ensure ess
  :bind
  ("M-=" . ess-insert-assign)
  :config

  (defcustom ess-gabe-style-list '((ess-indent-offset                . 2)
				   (ess-offset-arguments             . open-delim)
				   (ess-offset-arguments-newline     . prev-call)
				   (ess-offset-block                 . prev-line)
				   (ess-offset-continued             . straight)
				   (ess-align-nested-calls           . nil)
				   (ess-align-arguments-in-calls     . ("function[ \t]*("))
				   (ess-align-continuations-in-calls . nil)
				   (ess-align-blocks                 . (control-flow))
				   (ess-indent-from-lhs              . (arguments))
				   (ess-indent-from-chain-start      . nil)
				   (ess-indent-with-fancy-comments   . t))

    "Indentation variables for my own style"
    :group 'ess-edit
    :type 'alist
    :initialize 'custom-initialize-set
    :set (lambda (symbol value)
	   (set symbol value)
	   (ess-add-style 'GABE value)))
  (setq ess-style 'GABE)     ; can check options in 'ess-style-alist'

  ;;* Make shell read-only
  ;; (setq comint-prompt-read-only t)
    ;; https://cran.r-project.org/doc/manuals/R-ints.html#R-coding-standards
  (ess-toggle-underscore nil) ;disable underscore assign
  (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)
  (define-key ess-mode-map [(kp-enter)] 'ess-eval-region-or-line-and-step)
  ;;* Smart comma
  (setq ess-R-smart-operators t)
  ;;* Don't ask for working directory; use current
  (setq ess-ask-for-ess-directory nil)
  
  ;; (setq comint-scroll-to-bottom-on-input t)
  ;; ;; Easier access to past commands
  ;; ;; https://ess.r-project.org/Manual/ess.html#Command-History
  (eval-after-load "comint"
    '(progn
  ;;      ;; (define-key comint-mode-map [up]	;up arrow for previous input
  ;;      ;;   'comint-previous-matching-input-from-input)
  ;;      ;; (define-key comint-mode-map [down]	;down arrow for next input
  ;;      ;;   'comint-next-matching-input-from-input)

  ;;      ;; also recommended for ESS use --
  ;;      ;; (setq comint-move-point-for-output 'others) ;moves point to prompt when sending output
  ;;      ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
  ;;      ;; (setq comint-scroll-to-bottom-on-input 'this)
       (setq comint-scroll-to-bottom-on-input t) ;insertion and yank commands scroll the selected window to the bottom before inserting
       (setq comint-scroll-to-bottom-on-output t) ;scroll to bottom on output
       (setq comint-input-ignoredups t)	; controls whether successive identical inputs are stored in the input history
    )
  )

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
  ;;* Automatically set `options(width)` in R to window size
  (setq ess-auto-width 'window)
  ;; ;; Where to place windows
  ;; (setq display-buffer-alist
  ;; 	`(("*R Dired"
  ;; 	   (display-buffer-reuse-window display-buffer-in-side-window)
  ;; 	   (side . top)
  ;; 	   (slot . -1)
  ;; 	   ;; (window-width . 0.33)
  ;; 	   (reusable-frames . nil))
  ;; 	  ("*R view"
  ;; 	   (display-buffer-reuse-window display-buffer-in-side-window)
  ;; 	   (side . top)
  ;; 	   (slot . 1)
  ;; 	   ;; (window-width . 0.33)
  ;; 	   (reusable-frames . nil))
  ;; 	  ("*R"
  ;; 	   (display-buffer-reuse-window display-buffer-in-side-window)
  ;; 	   (side . right)
  ;; 	   (slot . 0)
  ;; 	   (window-width . 0.5)
  ;; 	   (reusable-frames . nil))
  ;; 	  ("*help"
  ;; 	   (display-buffer-reuse-window display-buffer-in-side-window)
  ;; 	   (side . right)
  ;; 	   (slot . -1)
  ;; 	   (window-height . 0.4)
  ;; 	   (reusable-frames . nil))))
  ;;
  ;; Syntax highlighting settings
  (setq ess-R-font-lock-keywords
        '(
          (ess-R-fl-keyword:keywords	.	t) ; default
          (ess-R-fl-keyword:constants	.	t) ; default
	  (ess-R-fl-keyword:modifiers	.	t) ; default
          (ess-R-fl-keyword:fun-defs	.	t) ; default
          (ess-R-fl-keyword:assign-ops	.	t) ; default
          (ess-R-fl-keyword:%op%	.	t)
          (ess-fl-keyword:fun-calls	.	nil)
          (ess-fl-keyword:numbers	.	nil)
          (ess-fl-keyword:operators	.	t)
          (ess-fl-keyword:delimiters	.	nil)
          (ess-fl-keyword:=		.	nil)
          (ess-R-fl-keyword:F&T	.	t))
	)
  ;; (setq inferior-R-font-lock-keywords
  ;;       '((ess-S-fl-keyword:prompt	.	t) ; default
  ;;         (ess-R-fl-keyword:messages	.	t) ; default
  ;;         (ess-R-fl-keyword:modifiers	.	t) ; default
  ;;         (ess-R-fl-keyword:fun-defs	.	t) ; default
  ;;         (ess-R-fl-keyword:keywords	.	t) ; default
  ;;         (ess-R-fl-keyword:assign-ops	.	t) ; default
  ;;         (ess-R-fl-keyword:constants	.	t) ; default
  ;;         (ess-fl-keyword:matrix-labels .	t) ; default
  ;;         (ess-fl-keyword:fun-calls	.	t)
  ;;         (ess-fl-keyword:numbers	.	t)
  ;;         (ess-fl-keyword:operators	.	t)
  ;;         (ess-fl-keyword:delimiters	.	t)
  ;;         (ess-fl-keyword:=		.	t)
  ;;         (ess-R-fl-keyword:F&T	.	t))
  ;; 	)
  ;; Additional highlighting trigger customizations
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (font-lock-add-keywords
		nil
		'(
		  ;; highlight named arguments
		  ("\\([(,]\\|[\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]"
		  ;; ("\\([(,][\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]\\|\2"
		   2 ess-numbers-face)

		  ;; highlight packages called through ::, :::
		  ("\\(\\w+\\):\\{2,3\\}" 1 ess-modifiers-face)
		  ;; highlight colons (:)
		  (":" . ess-operator-face)

		  ;; ;; highlight S4 methods
		  ;; ("\\(setMethod\\|setGeneric\\|setGroupGeneric\\|setClass\\|setRefClass\\|setReplaceMethod\\)" 1 font-lock-reference-face)
		  )
		)
	       )
	    )
  ;; Customize syntax highlighting colors
  (let ((numfg (face-attribute 'font-lock-type-face :foreground))
	(assignfg (face-attribute 'font-lock-constant-face :foreground))
	)
    (custom-set-faces
     `(ess-operator-face ((t (:inherit font-lock-variable-name-face))))
     `(ess-numbers-face ((t (:inherit font-lock-type-face :foreground ,(color-darken-name numfg 11)))))
     `(ess-constant-face ((t (:inherit font-lock-type-face :weight bold :foreground ,(color-lighten-name numfg 16)))))
     `(ess-keyword-face ((t (:inherit font-lock-keyword-face :weight bold))))
     ;; `(ess-function-call-face ((t (:inherit font-lock-function-name-face :weight normal))))
     `(ess-assignment-face ((t (:inherit font-lock-constant-face :foreground ,(color-lighten-name assignfg 8)))))
     )
    )
  
  ;; :init
  (setq ess-r-flymake-linters	
  	;; Available linters
  	'(
  	 ;; Syntax errors: reported by parse.
  	 ;; check that closures have the proper usage using codetools::checkUsage(). Note this runs base::eval() on the code, so do not use with untrusted code.
  	 "object_usage_linter = NULL" 
  	 ;; check that no absolute paths are used (e.g. "/var", "C:\System", "~/docs").
  	 "absolute_path_linter = NULL" 
  	 ;; check that file.path() is used to construct safe and portable paths.
  	 "nonportable_path_linter = NULL" 
  	 ;; Check that each step in a pipeline is on a new line, or the entire pipe fits on one line.
  	 "pipe_continuation_linter" ;ess default null
  	 ;; check that <- is always used for assignment
  	 "assignment_linter" 
  	 ;; check that objects are not in camelCase.
  	 "camel_case_linter = NULL" 
  	 ;; check that closed curly braces should always be on their own line unless they are followed by an else.
  	 "closed_curly_linter" ;ess default NULL
  	 ;; check that all commas are followed by spaces, but do not have spaces before them.
  	 "commas_linter" ;ess default null
  	 ;; check that there is no commented code outside of roxygen comments.
  	 "commented_code_linter = NULL" ;ess default null
  	 ;; check for overly complicated expressions.
  	 "cyclocomp_linter = NULL" 
  	 ;; check for x == NA
  	 "equals_na_linter = NULL" 
  	 ;; check that the [[ operator is used when extracting a single element from an object, not [ (subsetting) nor $ (interactive use).
  	 "extraction_operator_linter = NULL" 
  	 ;; check that all left parentheses in a function call do not have spaces before them.
  	 "function_left_parentheses_linter" 
  	 ;; check that integers are explicitly typed using the form 1L instead of 1.
  	 "implicit_integer_linter = NULL" 
  	 ;; check that all infix operators have spaces around them.
  	 "infix_spaces_linter" ;ess default null
  	 ;; check the line length of both comments and code is less than length.
  	 "line_length_linter = NULL" ;ess default null
  	 ;; check that only spaces are used, never tabs.
  	 "no_tab_linter = NULL" 
  	 ;; check that function and variable names are not more than length characters.
  	 "object_length_linter = NULL" ;ess default null
  	 ;; check that object names conform to a single naming style, e.g. CamelCase, camelCase, snake_case, SNAKE_CASE, dotted.case, lowercase, or UPPERCASE.
  	 "object_name_linter = NULL" ;ess default null
  	 ;; check that opening curly braces are never on their own line and are always followed by a newline.
  	 "open_curly_linter" ;ess default null
  	 ;; check that there is a space between right parenthesis and an opening curly brace.
  	 "paren_brace_linter" 
  	 ;; check that no semicolons terminate statements.
  	 "semicolon_terminator_linter = NULL" 
  	 ;; check for 1:length(...), 1:nrow(...), 1:ncol(...), 1:NROW(...), and 1:NCOL(...) expressions. These often cause bugs when the right hand side is zero. It is safer to use seq_len() or seq_along() instead.
  	 "seq_linter" 
  	 ;; check that only single quotes are used to delimit string constants.
  	 "single_quotes_linter" ;ess default null
  	 ;; check that parentheses and square brackets do not have spaces directly inside them.
  	 "spaces_inside_linter"  ;ess default null
  	 ;; check that all left parentheses have a space before them unless they are in a function call.
  	 "spaces_left_parentheses_linter"  ;ess default null
  	 ;; check that the source contains no TODO comments (case-insensitive).
  	 "todo_comment_linter = NULL" 
  	 ;; check there are no trailing blank lines.
  	 "trailing_blank_lines_linter = NULL"  ;ess default null
  	 ;; check there are no trailing whitespace characters.
  	 "trailing_whitespace_linter"  ;ess default null
  	 ;; avoid the symbols T and F (for TRUE and FALSE).
  	 "T_and_F_symbol_linter = NULL" 
  	 ;; report the use of undesirable functions, e.g. options or sapply and suggest an alternative.
  	 "undesirable_function_linter = NULL" 
  	 ;; report the use of undesirable operators, e.g. ::: or <<- and suggest an alternative.
  	 "undesirable_operator_linter = NULL" 
  	 ;; check that the c function is not used without arguments nor with a single constant.
  	 "unneeded_concatenation_linter"
  	 )
  	)
  
  )


;; (setq tramp-use-ssh-controlmaster-options nil)

;; (defun gabeFun (obj-name)
;;   (interactive "sPlot variable: ")
;;   (save-excursion
;;     (message "Plotting %s" obj-name)
;;     ;; (setq ess-plot-filename (make-temp-file "Rplot"))
;;     (setq ess-plot-filename (make-nearby-temp-file "Rplot"))
;;     ;; (ess-execute ess-command 'buffer nil nil)))
;;     ;; 
;;     ;; (ess-execute (format "print(%S)" (buffer-file-name)) 'buffer nil nil)))
;;     ;; (ess-execute (format "print(%s)" (read-string "Object Name:")))))
;;     ;; (ess-execute (concat "print(" obj-name ")") 'buffer)))
;;     (ess-execute (concat "png(tail(strsplit(\"" ess-plot-filename "\", \":\")[[1]], 1)); print(" obj-name "); dev.off()") 'buffer)
;;     (ess-wait-for-process)
;;     (select-frame (make-frame))
;;     (find-file (concat ess-plot-filename))))
;;     ;; (ess-execute (concat "print(\" " ess-plot-filename " \")") 'buffer)))


;; (defun gabeFun2 (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (message " Creating Plot")
;;     ;; (setq ess-plot-filename (make-temp-file "Rplot"))
;;     (setq ess-plot-filename (make-nearby-temp-file "Rplot"))
;;     ;; (ess-execute ess-command 'buffer nil nil)))
;;     ;; 
;;     ;; (ess-execute (format "print(%S)" (buffer-file-name)) 'buffer nil nil)))
;;     ;; (ess-execute (format "print(%s)" (read-string "Object Name:")))))
;;     ;; (ess-execute (concat "print(" obj-name ")") 'buffer)))
;;     (ess-execute (concat "png(tail(strsplit(\"" ess-plot-filename "\", \":\")[[1]], 1)); print(" (buffer-substring-no-properties start end) "); dev.off()") 'buffer)
;;     (ess-wait-for-process)
;;     (select-frame (make-frame))
;;     (find-file (concat ess-plot-filename))))
;;     ;; (ess-execute (concat "print(\" " ess-plot-filename " \")") 'buffer)))


;; Informed by:
;; https://stackoverflow.com/questions/10594208/how-do-i-get-the-region-selection-programmatically-in-emacs-lisp
(defun gabe-plot (start end)
  "Create temporary PNG file for ESS plot and open it in a new frame."
  (interactive (if (use-region-p)	; Get region, if there's a region selected
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and start end)
  (save-excursion
    (message "Creating Plot")
    ;; (setq ess-plot-filename (make-temp-file "Rplot"))
    ;; Create randomly named temporary file (in remote, if appropriate)
    (setq ess-plot-filename (make-nearby-temp-file "Rplot"))
    ;; (setq ess-plot-filename (concat (make-nearby-temp-file "Rplot") ".png"))
    ;; (ess-execute ess-command 'buffer nil nil)))
    ;; 
    ;; (ess-execute (format "print(%S)" (buffer-file-name)) 'buffer nil nil)))
    ;; (ess-execute (format "print(%s)" (read-string "Object Name:")))))
    ;; (ess-execute (concat "print(" obj-name ")") 'buffer)))
    ;; Run R code from region and plot into temp file
    ;; (ess-execute (concat "png(tail(strsplit('" ess-plot-filename "', ':')[[1]], 1), height = 800, width = 2000); print(" (buffer-substring-no-properties start end) "); dev.off()") 'buffer)
    (ess-execute (concat "png(tail(strsplit('" ess-plot-filename "', ':')[[1]], 1), height = 7, width = 11, units = 'in', res = 150); print(" (buffer-substring-no-properties start end) "); dev.off()") 'buffer)
    (ess-wait-for-process)		; Wait for plotting to finish
    ;; (select-frame (make-frame))
    ;; Create new frame
    (select-frame (make-frame '(
				(width . 145)
				(height . 35)
				(title . "R ESS Plot")
				)))
    (find-file (concat ess-plot-filename))) ; Open plot png in new frame
  (message "Error: No selection"))) ; Return error if no region selected
    ;; (ess-execute (concat "print(\" " ess-plot-filename " \")") 'buffer)))

;; (defun gabeFun (obj-name)
;;   (interactive "sPlot variable: ")
;;   (save-excursion
;;     (message "Plotting %s" obj-name)
;;     ;; (message "Plotting %s" (buffer-file-name))
;;     ;; (setq ess-command (format "print(%S)" (buffer-file-name)))
;;     ;; (ess-execute ess-command 'buffer nil nil)))
;;     ;; 
;;     ;; (ess-execute (format "print(%S)" (buffer-file-name)) 'buffer nil nil)))
;;     ;; (ess-execute (format "print(%s)" (read-string "Object Name:")))))
;;     (ess-execute (concat "print(" obj-name ")") 'buffer)))

;;;;;;;;;;;;;;;;;;;;
;; SQL

;; FUTURE LOOK INTO 
;; Consider looking into using https://github.com/purcell/sqlint (might need flycheck instead of flymake)
;; https://github.com/kostafey/ejc-sql
;; https://github.com/purcell/sqlformat

;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
;; http://www.bundesbrandschatzamt.de/~baron/blog/20200110-emacs-sql.html

;; http://web.mit.edu/Emacs/source/emacs-23.1/lisp/progmodes/sql.el
(use-package sql
  :defer
  :config
  ;; Don't wrap lines in comint
  (add-hook 'sql-interactive-mode-hook
	    (lambda ()
	      (toggle-truncate-lines t)
	      (horizontal-scroll-bar-mode t)
	      ))
  (setq sql-connection-alist
	'((safegraph (sql-product 'postgres)
		     (sql-port 5432)
		     (sql-server "localhost")
		     (sql-user "gabepsql")
		     (sql-password "Bga*3bYd0(lQpG")
		     (sql-database "gisdb"))
	  ;; (server2 (sql-product 'postgres)
	  ;; 	   (sql-port 5432)
	  ;; 	   (sql-server "localhost")
	  ;; 	   (sql-user "user")
	  ;; 	   (sql-password "password")
	  ;; 	   (sql-database "db2"))
	  ))
  ;; (defun my-sql-server2 ()
  ;;   (interactive)
  ;;   (my-sql-connect 'postgres 'server2))

  (defun sf-sql-connect ()
    (interactive)
    ;; remember to set the sql-product, otherwise, it will fail for the first time
    ;; you call the function
    (sql-set-product `postgres)
    ;; (setq sql-product 'postgres)
    (sql-connect 'safegraph))
  )

;; Change SQL keywords to uppercase automatically
(use-package sqlup-mode
  ;; https://github.com/Trevoke/sqlup-mode.el
  :hook (sql-mode sql-interactive-mode)
  )

;; SQL indentation
(use-package sql-indent
  ;; https://github.com/alex-hhh/emacs-sql-indent
  :hook (sql-mode . sqlind-minor-mode)
  :config
  ;; Left alignment; comment this out to right-align keywords
  (defvar my-sql-indentation-offsets-alist
    `((select-clause 0)
      (insert-clause 0)
      (delete-clause 0)
      (update-clause 0)
      ,@sqlind-default-indentation-offsets-alist))
  (setq sqlind-indentation-offsets-alist my-sql-indentation-offsets-alist)
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

  (sp-local-pair 'poly-r-sql-mode "/* Start SQL Query */" "/* End SQL Query */"
		 :trigger "\\sql")
  )

;; (use-package smartparens-mode
;;   ;; https://github.com/Fuco1/smartparens
;;   ;; https://media.readthedocs.org/pdf/smartparens/latest/smartparens.pdf
;;   ;; :disabled
;;   :ensure smartparens
;;   :hook (prog-mode ess-r-mode inferior-ess-mode)
;;   :init
;;   ;; (require `smartparens-ess)
;;   (require `smartparens-config)
;;   (turn-on-smartparens-strict-mode)
;;   (turn-on-show-smartparens-mode)
;;   ;; (show-smartparens-global-mode)
;;   ;; (setq sp-show-pair-from-inside 1)
;;   (setq sp-highlight-wrap-overlay nil)
;;   (setq sp-highlight-wrap-tag-overlay nil)



;;   (dolist (mode '(ess-mode ess-r-mode inferior-ess-mode inferior-ess-r-mode))
;;     ;; avoid traveling commas when slurping
;;     ;; (|a, b), c ---> (|a, b, c)
;;     (add-to-list 'sp-sexp-suffix (list mode 'regexp ""))
;;     ;; `sp-sexp-prefix' for ESS
;;     (add-to-list 'sp-sexp-prefix
;; 		 (list mode 'regexp
;; 		       (rx (zero-or-more (or word (syntax symbol)))))))
;;   ;; 
;;   ;; slurping follows Google's R style guide
;;   ;; see https://google.github.io/styleguide/Rguide.xml
;;   (defun sp-ess-pre-handler (_id action _context)
;;     "Remove spaces before opening parenthesis in a function call.
;; Remove redundant space around commas.
;; ID, ACTION, CONTEXT."
;;     (when (equal action 'slurp-forward)
;;       (let ((sxp (sp-get-thing 'back)))
;; 	(save-excursion
;; 	  (goto-char (sp-get sxp :beg-prf))
;; 	  ;; (|)   x ---> (x)
;; 	  (when (looking-back (rx (syntax open-parenthesis)
;; 				  (one-or-more space)) nil)
;; 	    (cycle-spacing 0 nil 'single-shot))
;; 	  (cond
;; 	   ;; (|)if(cond) ---> (|if (cond))
;; 	   ((member (sp-get sxp :prefix) '("if" "for" "while"))
;; 	    (goto-char (sp-get sxp :beg))
;; 	    (cycle-spacing 1 nil 'single-shot))
;; 	   ;; (|)v [,2] <- if(x > 1) ---> (v[,2] <- if (x > 1))
;; 	   ((and
;; 	     (member (sp-get sxp :op) '("[" "("))
;; 	     (equal (sp-get sxp :prefix) "")
;; 	     (looking-back
;; 	      (rx (and (not-char "%" ",")
;; 		       (not (syntax close-parenthesis)))
;; 		  (one-or-more space)) nil)
;; 	     (not (member
;; 		   (save-excursion
;; 		     (sp-backward-sexp)
;; 		     (thing-at-point 'word 'noprop))
;; 		   '("if" "for" "while"))))
;; 	    (cycle-spacing 0 nil 'single-shot))
;; 	   ;; (|[...])%in% ---> ([...] %in%|)
;; 	   ((or (looking-at "%") (looking-back "%" nil))
;; 	    (just-one-space))
;; 	   ;; (|)a , b,    c ---> (|a, b, c)
;; 	   ((looking-back
;; 	     (rx (zero-or-more space) "," (zero-or-more space))
;; 	     (line-beginning-position) 'greedy)
;; 	    (replace-match ", "))))))
;;     (when (equal action 'slurp-backward)
;;       (let ((sxp (sp-get-thing)))
;; 	(save-excursion
;; 	  (goto-char (sp-get sxp :end))
;; 	  ;; x  (|) ---> (x)
;; 	  (when (looking-at (rx (one-or-more space)
;; 				(syntax close-parenthesis)))
;; 	    (cycle-spacing 0 nil 'single-shot))
;; 	  ;; if(cond){} (|) ---> (if (cond) {}|)
;; 	  (cond ((member (sp-get sxp :prefix) '("if" "for" "while"))
;; 		 (goto-char (sp-get sxp :beg))
;; 		 (cycle-spacing 1 nil 'single-shot))
;; 		;; for style reasons there should be a space before curly
;; 		;; brackets and binary operators
;; 		((and (member (sp-get sxp :op) '("{" "%"))
;; 		      (not (looking-at (rx (syntax close-parenthesis)))))
;; 		 (cycle-spacing 1 nil 'single-shot))
;; 		;; v[2](|) ---> (v[2]|)
;; 		((and
;; 		  (not (member (thing-at-point 'word 'noprop)
;; 			       '("if" "for" "while")))
;; 		  (looking-at
;; 		   (rx (and (zero-or-more space)
;; 			    (not-char "{")
;; 			    (or (syntax close-parenthesis)
;; 				(char "(")
;; 				(char "["))))))
;; 		 (cycle-spacing 0 nil 'single-shot))
;; 		;; 1 , 2 (|) ---> (1, 2)
;; 		((looking-at
;; 		  (rx (zero-or-more space) "," (zero-or-more space)))
;; 		 (replace-match ", ")))))))
;;   ;; 
;;   ;; function(x) {|} ---> function(x) {\n|\n}
;;   ;; ##' \tabular{rrr}{|} --->
;;   ;; ##' \tabular{rrr}{
;;   ;; ##'   |
;;   ;; ##' }
;;   (defun sp-ess-open-sexp-indent (&rest _args)
;;     "Open new brace or bracket with indentation.
;; ARGS."
;;     (if (and (fboundp 'ess-roxy-entry-p) (ess-roxy-entry-p))
;; 	(progn
;; 	  (save-excursion (ess-roxy-indent-on-newline))
;; 	  (when (looking-back ess-roxy-str nil)
;; 	    (cycle-spacing 3 nil t)))
;;       (newline)
;;       (indent-according-to-mode)
;;       (forward-line -1)
;;       (indent-according-to-mode)))
;;   ;; 
;; (defun sp-ess-roxy-str-p (_id action _context)
;;   "Test if looking back at `ess-roxy-re'.
;; ID, ACTION, CONTEXT."
;;   (when (and (boundp 'ess-roxy-re) (eq action 'insert))
;;     (sp--looking-back-p ess-roxy-re)))
;; ;; 
;; (sp-with-modes 'ess-r-mode
;;   (sp-local-pair "{" nil
;;                  :pre-handlers '(sp-ess-pre-handler)
;;     ;; the more reasonable C-j interferes with default binding for
;;     ;; `ess-eval-line'
;;                  :post-handlers '((sp-ess-open-sexp-indent "M-j")))
;;   (sp-local-pair "(" nil
;;                  :pre-handlers '(sp-ess-pre-handler)
;;                  :post-handlers '((sp-ess-open-sexp-indent "M-j")))
;;   (sp-local-pair "[" nil
;;                  :pre-handlers '(sp-ess-pre-handler)
;;                  :post-handlers '((sp-ess-open-sexp-indent "M-j")))
;;   (sp-local-pair "'" nil
;;                  :unless '(sp-ess-roxy-str-p sp-in-comment-p sp-in-string-quotes-p)))


  
  ;; ;; :init
  ;; (sp-local-pair 'ess-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  ;; (defun my-create-newline-and-enter-sexp (&rest _ignored)
  ;;   "Open a new brace or bracket expression, with relevant newlines and indent. "
  ;;   (newline)
  ;;   (indent-according-to-mode)
  ;;   (forward-line -1)
  ;;   (indent-according-to-mode))
  
  ;; )


(use-package company
  ;; :disabled
  ;; :bind
  ;; ("TAB" . company-indent-or-complete-common)
  :config
  (global-company-mode t)
  (global-set-key (kbd "C-c c") 'company-complete)
  (setq company-idle-delay 0.3;1.5
	company-minimum-prefix-length 2
	;; company-auto-complete t
	;; From: https://emacs.stackexchange.com/questions/35345/how-to-turn-off-autocompletion-for-numbers-and-numbers-only-in-company-mode-in
	company-dabbrev-char-regexp "[A-z:-]"
	)
  ;; Popups
  ;; (company-quickhelp-mode)
  ;; (setq company-quickhelp-delay nil)
  ;; (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  ;; (define-key company-quickhelp-mode-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  
   ;; (custom-set-faces
  ;; '(company-preview
  ;;   ((t (:foreground "darkgray" :underline t))))
  ;; '(company-preview-common
  ;;   ((t (:inherit company-preview))))
  ;; '(company-tooltip
  ;;   ((t (:background "lightgray" :foreground "black"))))
  ;; '(company-tooltip-selection
  ;;   ((t (:background "steelblue" :foreground "white"))))
  ;; '(company-tooltip-common
  ;;   ((((type x)) (:inherit company-tooltip :weight bold))
  ;;    (t (:inherit company-tooltip))))
  ;; '(company-tooltip-common-selection
  ;;   ((((type x)) (:inherit company-tooltip-selection :weight bold))
  ;;    (t (:inherit company-tooltip-selection))))
  ;; )
  
  (require 'color)
  
  (let ((bg (face-attribute 'default :background))
	(fg (face-attribute 'default :foreground)))
    (custom-set-faces
     ;; `(company-preview ((t (:foreground ,(color-darken-name fg 15) :background ,(color-lighten-name bg 10)))))
     `(company-preview ((t (:foreground ,(color-darken-name fg 7) :background "steelblue"))))
     '(company-preview-common
       ((t (:inherit company-preview))))
     ;; `(company-tooltip ((t (:inherit default :foreground ,(color-darken-name fg 25) :background ,(color-lighten-name bg 15)))))
     `(company-tooltip
       ((t (:inherit default :foreground "white" :background ,(color-lighten-name bg 15)))))
     '(company-tooltip-selection
       ;; ((t (:background "steelblue" :foreground "white"))))
       ((t (:background "steelblue"))))
     `(company-tooltip-common ((t (:foreground "orange1"))))
     ;; 
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     ;; `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     ;; `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
     )
    )
  )

(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode)
  )

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay nil) 
  ;; (define-key company-quickhelp-mode-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (setq company-quickhelp-use-propertized-text t) 

  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (setq company-quickhelp-color-background "white"
	  company-quickhelp-color-foreground (color-lighten-name bg 15)
	  )
    )

  )


(use-package poly-R
  ;; :disabled
  :ensure t
  )

(use-package poly-markdown
  ;; :disabled
  ;; :defer
  :ensure t
  )

(use-package polymode
  ;; :mode ("\\.R\\$" . poly-r-sql-mode)
  :defer
  ;; :hook (ess-r-mode . poly-r-sql-mode)
  :config
  ;; (setq polymode-prefix-key (kbd "C-c n"))
  ;; (define-hostmode poly-R-hostmode :mode 'R-mode)
  (define-innermode poly-sql-expr-r-innermode
    :mode 'sql-mode
    :head-matcher "/\\* start sql \\*/"
    :tail-matcher "/\\* end sql \\*/"
    :head-mode 'body
    :tail-mode 'body
    )
  (define-polymode poly-r-sql-mode
    :hostmode 'poly-R-hostmode
    :innermodes '(poly-sql-expr-r-innermode)
    ;; (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    ;; (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)
    )
  )

(use-package electric-operator
  ;; https://github.com/davidshepherd7/electric-operator
  ;; :disabled
  :hook (ess-r-mode. electric-operator-mode)
  )

;; (use-package electric-layout-mode
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Misc-for-Programs.html
;;   :ensure nil
;;   :hook (ess-r-mode. electric-operator-mode)
;;   )

(use-package flyspell
  ;; :disabled
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  )

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  )

(use-package highlight-indent-guides
  ;; https://github.com/DarthFennec/highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  )

;; (use-package sr-speedbar
;; https://www.emacswiki.org/emacs/SrSpeedbar
;; )

;; Syntax checking
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  ;; (setq flymake-diagnostic-at-point-timer-delay 0.7)
  )

;; (use-package
;;   )

;; (use-package flycheck
;;   ;; https://www.flycheck.org/en/latest/
;;   :disabled
;;   :init (global-flycheck-mode)
;;   )
;; (use-package flycheck-inline
;;   ;; https://github.com/flycheck/flycheck-inline
;;   :disabled
;;   )
;; (use-package flycheck-color-mode-line
;;   :disabled)
;; (use-package flycheck-pos-tip
;;   :disabled)
;; (use-package flycheck-bashate
;;   :disabled
;;   :defer
;;   )
;; (use-package flycheck-checkbashisms
;;   :disabled
;;   :defer
;;   )
;; ;; (use-package flycheck-title
;; ;;   :disabled
;; ;;   )


(use-package company-reftex
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
(use-package ein
  :disabled)
(use-package smart-forward
  :disabled)
(use-package yasnippet-snippets
  :disabled)
(use-package yasnippet
  :disabled)
(use-package auctex
  :disabled)
;; (use-package use-package
;;   :disabled)

                       


;; Original value of ess-r-flymake-linters for ESS lintr linters
;; ("closed_curly_linter = NULL" "commas_linter = NULL" "commented_code_linter = NULL" "infix_spaces_linter = NULL" "line_length_linter = NULL" "object_length_linter = NULL" "object_name_linter = NULL" "object_usage_linter = NULL" "open_curly_linter = NULL" "pipe_continuation_linter = NULL" "single_quotes_linter = NULL" "spaces_inside_linter = NULL" "spaces_left_parentheses_linter = NULL" "trailing_blank_lines_linter = NULL" "trailing_whitespace_linter = NULL")


;; ;; Available linters
;; (
;;  ;; Syntax errors: reported by parse.
;;  ;; check that closures have the proper usage using codetools::checkUsage(). Note this runs base::eval() on the code, so do not use with untrusted code.
;;  "object_usage_linter = NULL" 
;;  ;; check that no absolute paths are used (e.g. "/var", "C:\System", "~/docs").
;;  "absolute_path_linter = NULL" 
;;  ;; check that file.path() is used to construct safe and portable paths.
;;  "nonportable_path_linter = NULL" 
;;  ;; Check that each step in a pipeline is on a new line, or the entire pipe fits on one line.
;;  "pipe_continuation_linter" ;ess default null
;;  ;; check that <- is always used for assignment
;;  "assignment_linter" 
;;  ;; check that objects are not in camelCase.
;;  "camel_case_linter = NULL"
;;  ;; check that closed curly braces should always be on their own line unless they are followed by an else.
;;  "closed_curly_linter" ;ess default NULL
;;  ;; check that all commas are followed by spaces, but do not have spaces before them.
;;  "commas_linter" ;ess default null
;;  ;; check that there is no commented code outside of roxygen comments.
;;  "commented_code_linter = NULL" ;ess default null
;;  ;; check for overly complicated expressions.
;;  "cyclocomp_linter = NULL"
;;  ;; check for x == NA
;;  "equals_na_linter = NULL"
;;  ;; check that the [[ operator is used when extracting a single element from an object, not [ (subsetting) nor $ (interactive use).
;;  "extraction_operator_linter = NULL"
;;  ;; check that all left parentheses in a function call do not have spaces before them.
;;  "function_left_parentheses_linter"
;;  ;; check that integers are explicitly typed using the form 1L instead of 1.
;;  "implicit_integer_linter = NULL"
;;  ;; check that all infix operators have spaces around them.
;;  "infix_spaces_linter" ;ess default null
;;  ;; check the line length of both comments and code is less than length.
;;  "line_length_linter = NULL" ;ess default null
;;  ;; check that only spaces are used, never tabs.
;;  "no_tab_linter = NULL"
;;  ;; check that function and variable names are not more than length characters.
;;  "object_length_linter = NULL" ;ess default null
;;  ;; check that object names conform to a single naming style, e.g. CamelCase, camelCase, snake_case, SNAKE_CASE, dotted.case, lowercase, or UPPERCASE.
;;  "object_name_linter" ;ess default null
;;  ;; check that opening curly braces are never on their own line and are always followed by a newline.
;;  "open_curly_linter" ;ess default null
;;  ;; check that there is a space between right parenthesis and an opening curly brace.
;;  "paren_brace_linter"
;;  ;; check that no semicolons terminate statements.
;;  "semicolon_terminator_linter = NULL"
;;  ;; check for 1:length(...), 1:nrow(...), 1:ncol(...), 1:NROW(...), and 1:NCOL(...) expressions. These often cause bugs when the right hand side is zero. It is safer to use seq_len() or seq_along() instead.
;;  "seq_linter"
;;  ;; check that only single quotes are used to delimit string constants.
;;  "single_quotes_linter" ;ess default null
;;  ;; check that parentheses and square brackets do not have spaces directly inside them.
;;  "spaces_inside_linter"  ;ess default null
;;  ;; check that all left parentheses have a space before them unless they are in a function call.
;;  "spaces_left_parentheses_linter"  ;ess default null
;;  ;; check that the source contains no TODO comments (case-insensitive).
;;  "todo_comment_linter = NULL" 
;;  ;; check there are no trailing blank lines.
;;  "trailing_blank_lines_linter = NULL"  ;ess default null
;;  ;; check there are no trailing whitespace characters.
;;  "trailing_whitespace_linter"  ;ess default null
;;  ;; avoid the symbols T and F (for TRUE and FALSE).
;;  "T_and_F_symbol_linter = NULL" 
;;  ;; report the use of undesirable functions, e.g. options or sapply and suggest an alternative.
;;  "undesirable_function_linter" 
;;  ;; report the use of undesirable operators, e.g. ::: or <<- and suggest an alternative.
;;  "undesirable_operator_linter" 
;;  ;; check that the c function is not used without arguments nor with a single constant.
;;  "unneeded_concatenation_linter"
;;  )


;; https://emacs.stackexchange.com/questions/585/split-window-at-outermost-border
;; 
;; (defun my-split-main-window (direction)
;;   "Split the main window in the DIRECTION where DIRECTION is a symbol with
;; possible values of right, left, above or below and SIZE is the final size of the
;; windows, if the window is split horizontally (i.e. in DIRECTION below or above)
;; SIZE is assumed to be the target height otherwise SIZE is assumed to be the
;; target width"
;;   (interactive "sDirection:")
;;   (let* ((new-window (split-window (frame-root-window) nil direction))
;;          (horizontal (member direction '(right left))))
;;     (save-excursion 
;;       (select-window new-window)
;;       ;; (enlarge-window (- size (if horizontal
;;       ;;                             (window-width)
;;       ;;                           (window-height)))
;;       ;;                 horizontal)
;;       )
;;     new-window))
