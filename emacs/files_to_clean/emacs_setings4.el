;; .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Okular"
      ("okular --unique %o#src:%n%b")
      "okular"))))
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Okular")
     ((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(custom-enabled-themes (quote (misterioso)))
 '(diff-switches "-u")
 '(flycheck-lintr-caching nil)
 '(global-whitespace-mode t)
 '(inferior-R-args "--no-save --no-restore")
 '(inhibit-startup-screen t)
 '(markdown-command "markdown2")
 '(reftex-toc-split-windows-horizontally t)
 '(show-trailing-whitespace nil)
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-global-modes (quote (prog-mode ess-mode)))
 '(whitespace-style (quote (face trailing newline indentation empty))))

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-empty ((t (:background "dim gray" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "brown" :foreground "white smoke")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; MELPA Library
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; To create a list of current packages
;; http://stackoverflow.com/questions/13866848/how-to-save-a-list-of-all-the-installed-packages-in-emacs-24
;; To download any packages if needed
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERAL CONFIG OPTIONS
;;* Global options to set up mist
;;
;;* Set default Emacs frame size
;; (setq default-frame-alist '((width . 90) (height . 25)))
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))
;;* Show line number in the mode line
(setq line-number-mode t)
;;* Show column number in the mode line
(setq column-number-mode t)
;;* Focus follows mouse
(setq mouse-autoselect-window t)
;;* Change threshold for automatic horizontal split
(setq split-width-threshold 100)
;;* Delete highlighted text when typing
(delete-selection-mode t)
;;* Highlight matching parentheses
(show-paren-mode t)
;;* Show line number on left side of window
;;(global-linum-mode t)
;; Disable toolbar
(tool-bar-mode -1)
;;
;; 
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ess-use-auto-complete t)
;; (require 'ess-eldoc)
;; (setq ess-use-auto-complete 'script-only)
;; 

;;;;;;;;;;;;;************* Fix these
(define-key mode-line-buffer-identification-keymap [mode-line mouse-2] 'buffer-menu)
(setq whitespace-style '(face lines-tail trailing empty))
;; (add-hook 'prog-mode-hook
;; 	  (function (lambda ()
;; 		      (whitespace-mode 't))))
(global-whitespace-mode t)
(setq whitespace-global-modes '(prog-mode ess-mode))

;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-12" ))
;; (add-to-list 'default-frame-alist '(font . "Anonymous Pro-14" ))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-13:weight=regular" ))
;;;;;;;;;;;;;****************


;; to avoid pasting when correcting word
;; modified from: http://emacs.stackexchange.com/questions/13361/how-do-i-disable-the-emacs-flyspell-middle-mouse-correction
(eval-after-load "flyspell"
  '(progn
     ;; (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     ;; (define-key flyspell-mouse-map [mouse-3] #'undefined)
     ;; (define-key flyspell-mouse-map [down-mouse-2] nil)
     ;; (define-key flyspell-mouse-map [mouse-2] nil)))
     (define-key flyspell-mouse-map [down-mouse-2] nil)
     (define-key flyspell-mouse-map [mouse-2] #'flyspell-correct-word)))

;;;;;;;;;;
;;;;;;;;;;;;;;

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)
;;;;
;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; (setq-default ac-sources (push 'ac-source-yasnippet ac-sources))
    ;; (defadvice TeX-insert-quote (around wrap-region activate)
    ;;   (cond
    ;;    (mark-active
    ;;     (let ((skeleton-end-newline nil))
    ;;       (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
    ;;    ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
    ;;     (forward-char (length TeX-open-quote)))
    ;;    (t
    ;;     ad-do-it)))
    ;; (put 'TeX-insert-quote 'delete-selection nil)
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DEFINE KEYBINDINGS
;; Both global and per-mode
;;
;; ESS Keybinds
(with-eval-after-load 'ess
  (setq ess-S-assign-key (kbd "M-="))	; Change assign key
  (ess-toggle-S-assign-key t)     ; enable above key definition
  (ess-toggle-underscore nil)	  ;disable underscore assign
  (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)
  (define-key inferior-ess-mode-map [C-up] 'comint-previous-input)
  (define-key inferior-ess-mode-map [C-down] 'comint-next-input)
  ;; (add-hook 'Rnw-mode-hook
  ;;           '(lambda()
  ;;              (local-set-key [(shift return)]
  ;;              'ess-eval-region-or-line-and-step)))
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ess-use-auto-complete t)
;; (require 'ess-eldoc)
;; (setq ess-use-auto-complete 'script-only)
  ;; Help popup shortcut
  ;; (define-key ac-completing-map [C-c h] 'ac-quick-help)
  ;; (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  ;; Auto-Complete commands
  ;; (setq ac-auto-start 3)
  ;; (define-key ac-completing-map [(tab)] 'ac-complete)
  ;; (define-key ac-completing-map [(return)] nil)
  ;; 
 )

;; AUTOCOMPLETE
;; Avoid bug that doesn't initialize auto-complete with flyspell enabled
(ac-flyspell-workaround)
;; Automatically start auto-complete (if number, after n number of characters)
(setq ac-auto-start 3)
;; Auto-completion menu (i.e. suggestions)
(setq ac-auto-show-menu nil)
;; Auto-completion tooltip
;; (setq ac-use-quick-help nil)
;; Use auto-completion history
(setq ac-use-comphist nil)
;; Hotkey to start auto-complete
(ac-set-trigger-key "TAB")
;; Disable completion on TAB *while* completing and multiple candidates
(define-key ac-completing-map (kbd "TAB") nil)
;; Stop completion with RET
(define-key ac-completing-map (kbd "RET") 'ac-stop)
;; (define-key ac-mode-map (kbd "RET") nil)
;; (define-key ac-completing-map [return] nil)
;; (define-key ac-mode-map (kbd "TAB") 'ac-complete)
;; (define-key ac-mode-map [(tab)] 'ac-complete)
;; (define-key ac-mode-map  'ac-quick-help)
;; (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMINT CONFIG
;; Options for interactive shell (e.g. R, python, etc)
;;
;;* Make shell read-only
;; (setq comint-prompt-read-only t)
;;* Scroll on input/output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;;* Color in shell
(setq ansi-color-for-comint-mode 'filter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS LISP CONFIG
;; Setup elisp options
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '(nil "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FORTRAN 90 CONFIG
;; Setup f90-mode options
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ESS CONFIG
;; Setup ess-mode options
;;
(with-eval-after-load 'ess
  ;;* Smart comma
  (setq ess-R-smart-operators t)
  ;;* Don't ask for working directory; use current
  (setq ess-ask-for-ess-directory nil)
  ;;* Name *R* interactive buffer
  (setq ess-local-process-name "R")
  ;;* Create a new frame for each help instance
  ;; (setq ess-help-own-frame t)
  ;;* All help buffers go into single new frame
  (setq ess-help-own-frame 'one)
  ;;* Remove trailing whitespace after each line
  ;; (setq ess-nuke-trailing-whitespace-p 'ask) ;ask for confirmation
  (setq ess-nuke-trailing-whitespace-p t) ;automatic
  ;;* Set indentation level; default = 2, R-core recommends 4
  ;;(setq ess-indent-level 4)
  ;;* From http://cran.r-project.org/doc/manuals/r-release/R-ints.html
  (add-hook 'ess-mode-hook
	    (lambda ()
	      ;; (ess-set-style 'C++ 'quiet)
	      (ess-set-style 'GNU 'quiet)
	      ;; Because
	      ;;                                 DEF GNU BSD K&R C++
	      ;; ess-indent-level                  2   2   8   5   4
	      ;; ess-continued-statement-offset    2   2   8   5   4
	      ;; ess-brace-offset                  0   0  -8  -5  -4
	      ;; ess-arg-function-offset           2   4   0   0   0
	      ;; ess-expression-offset             4   2   8   5   4
	      ;; ess-else-offset                   0   0   0   0   0
	      ;; ess-close-brace-offset            0   0   0   0   0
	      (add-hook 'local-write-file-hooks
			(lambda ()
			  (ess-nuke-trailing-whitespace)))))
  ;; (add-hook 'ess-mode-hook 'electric-spacing-mode)
  (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook 'smartparens-mode)
  ;; (add-hook 'inferior-ess-mode-hook 'electric-spacing-mode)
  (add-hook 'inferior-ess-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'inferior-ess-mode-hook 'smartparens-mode)
  (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)
  ;;
  ;; Should move this to it's own section 
  (require 'smartparens)
  (sp-local-pair 'ess-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
  ;; 
  )
;;(require 'smartparens-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LATEX CONFIG
;; Setup LaTex options
;;
;;(add-hook 'latex-mode-hook 'auto-fill-mode) ;wrap lines in paragraph
;; LaTex
;; (add-hook 'latex-mode-hook 'turn-on-auto-fill)
;; (add-hook 'latex-mode-hook 'smartparens-mode)
;; AucTeX options to compile BibTeX
(with-eval-after-load 'latex
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MARKDOWN CONFIG
;; Setup markdown options
;;
;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLYSPELL CONFIG
;; Automatic spell check
;;
;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (flyspell-prog-mode)))
;; Fortran 90
(add-hook 'f90-mode-hook
	  (lambda () (flyspell-prog-mode)))
;; ESS
(add-hook 'ess-mode-hook
	  (lambda () (flyspell-prog-mode)))
;; LaTex
(add-hook 'latex-mode-hook 'flyspell-mode)
;; Markdown
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLYCHECK CONFIG
;; Automatic code syntax check
;;
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (flycheck-mode t)))
;; Fortran 90
;; (add-hook 'f90-mode-hook
;; 	  (lambda () (flycheck-mode t)))
;; ESS
(with-eval-after-load 'ess
  ;; line below no longer needed, already bundled with flycheck
  ;; (load "/home/gcaceres/Software/R/R_libs/lintr/flycheck/lintr.el")
  ;; seems '(flycheck-lintr-caching nil) is needed to avoid error
  ;; but doesn't work... change through customize-emacs
  (add-hook 'ess-mode-hook
	    (lambda () (flycheck-mode t)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AUTOFILL CONFIG
;; Automatically insert new line instead of wrapping text
;;
;;* Function to auto-fill comments in code
;; (defun comment-auto-fill ()
;;   (setq-local comment-auto-fill-only-comments t)
;;   (auto-fill-mode 1))
;; ;; Fortran 90
;; (add-hook 'f90-mode-hook 'turn-on-auto-fill)
;; ;; LaTex
;; (add-hook 'latex-mode-hook 'turn-on-auto-fill)
;; ;; Emacs Lisp
;; (add-hook 'emacs-lisp-mode-hook 'comment-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ElECTRIC SPACING CONFIG
;; Insert spaces around operators
;;
;;* Change some spacing rules
(with-eval-after-load 'electric-spacing
  ;; No electric spacing in comments or strings (does it work??)
  ;; (setq electric-spacing-docs nil)
  ;; Updated version for ':' from github including option for ess-mode
  (defun electric-spacing-: ()
    "See `electric-spacing-insert'."
    (cond (c-buffer-is-cc-mode
	   (if (looking-back "\\?.+")
	       (electric-spacing-insert ":")
	     (electric-spacing-insert ":" 'middle)))
	  ((derived-mode-p 'haskell-mode)
	   (electric-spacing-insert ":"))
	  ((derived-mode-p 'python-mode) (electric-spacing-python-:))
	  ((derived-mode-p 'ess-mode)
	   (insert ":"))
	  (t
	   (electric-spacing-insert ":" 'after))))
  ;; Modify '?' in ess-mode
  (defun electric-spacing-? ()
    "See `electric-spacing-insert'."
    (cond (c-buffer-is-cc-mode
	   (electric-spacing-insert "?"))
	  ((derived-mode-p 'ess-mode)
	   (insert "?"))
	  (t
	   (electric-spacing-insert "?" 'after))))
  ;; Don't mess with periods
  (defun electric-spacing-. ()
    "See `electric-spacing-insert'."
    (insert "."))
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; SMARTPARENS CONFIG
;; ;; Automatically insert pairs
;; ;;
;; ;; Create block when using curly brackets
;; (require 'smartparens-config)
;; (sp-local-pair 'ess-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
;; (defun my-create-newline-and-enter-sexp (&rest _ignored)
;;   ;; "Open a new brace or bracket expression, with relevant newlines and indent. "
;;   (newline)
;;   (indent-according-to-mode)
;;   (forward-line -1)
;;   (indent-according-to-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS mode
;; Quick help
;; (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)

;; ;; General Config
;; (load-file "~/.emacs.d/my_config/general_config.el")

;; ;; Markdown Config
;; (load-file "~/.emacs.d/my_config/markdown_config.el")

;; ;; Flycheck Config
;; (load-file "~/.emacs.d/my_config/flycheck_config.el")

;; ;; ESS Config
;; (load-file "~/.emacs.d/my_config/ess_config.el")

;;(load-file "~/.emacs.d/my_config/rippedcasts-theme.el")

