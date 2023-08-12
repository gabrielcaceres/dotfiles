;;;;;;;; ESS CONFIG

;; Don't ask for working directory; use current 
(setq ess-ask-for-ess-directory nil) 
;; Name *R* interactive buffer
(setq ess-local-process-name "R")
;; Color in R shell
(setq ansi-color-for-comint-mode 'filter)
;; Make R shell read-only
;; (setq comint-prompt-read-only t)
;; Scroll on input/output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; create a new frame for each help instance
;; (setq ess-help-own-frame t)
;; If you want all help buffers to go into one frame do:
(setq ess-help-own-frame 'one)

;; Change assign key
(setq ess-smart-S-assign-key "M-=")

;; Non-nil means automatically newline before and after braces inserted in S code.
;;(setq ess-auto-newline t)

;; Set indentation level; default = 2, R-core recommends 4
;;(setq ess-indent-level 4)
;;; From http://cran.r-project.org/doc/manuals/r-release/R-ints.html
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'C++ 'quiet)
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
(setq ess-nuke-trailing-whitespace-p 'ask)
;; or even
;; (setq ess-nuke-trailing-whitespace-p t)

(define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)
(define-key inferior-ess-mode-map [C-up] 'comint-previous-input)
(define-key inferior-ess-mode-map [C-down] 'comint-next-input)

;; Activate flyspell for comments in code
(add-hook 'ess-mode-hook
          (lambda () (flyspell-prog-mode)))

;;;Automatically use lintr to check R files. Note you will need to change the path to where you put lintr.el:
(require 'flycheck)
(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode t)))
(load "/home/gcaceres/Software/R/R_libs/lintr/flycheck/lintr.el")

(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'ess-eval-region-or-line-and-step)))
(require 'ess-site)
