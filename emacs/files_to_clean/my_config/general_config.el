;;;; GENERAL CONFIG

;; Set default Emacs frame size
;; (setq default-frame-alist '((width . 90) (height . 25)))
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))

;; Focus follows mouse
(setq mouse-autoselect-window t)

;; Change threshold for automatic horizontal split
(setq split-width-threshold 10)

;; Delete highlighted text when typing
(delete-selection-mode t)

;; Highlight matching parentheses
(show-paren-mode t)

;; Show line number on left side of window
;;(global-linum-mode 1) 
