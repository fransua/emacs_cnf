(setq load-path (cons "~/.emacs.dev/emacs.d/" load-path))

(setq load-path (cons (expand-file-name "~/.emacs.dev/emacs.d/lisp")
		      load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; python
; apt-get install ipython
; apt-get install pymacs
; apt-get install pylint
; pylint --generate-rcfile > ~/.pylintrc #warnings...
; apt-get install python-mode

; if file ends with .py -> python mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(require 'ipython)
(add-hook 'python-mode-hook 'my-python-hook)
; this getz called after python mode is enabled
(defun my-python-hook ()
  ; Shift + arrows indent
  (local-set-key [\M-\right] 'py-shift-region-right)
  (local-set-key [\M-\left]  'py-shift-region-left)
  (show-paren-mode 1)
)

;templates
(require 'template)
(template-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; general
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(pc-selection-mode t) ; to select with mouse/shift+arrow
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(inhibit-startup-screen t)
 '(truncate-lines t)
 '(show-paren-mode t)
 '(setq visible-bell t)
 )

; key-bindings
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown"))))
; '(flymake-errline ((((class color)) (:background "LightPink" :foreground "black"))))
 '(flymake-warnline             ((((class color)                  ) (:underline "yellow"                    ))                                                   ))
 '(font-lock-builtin-face       ((((class color) (background dark)) (:foreground "cyan"                     )) (((class color)) (:foreground "SkyBlue" :bold t ))))
 '(font-lock-comment-face       ((((class color) (background dark)) (:foreground "grey50" :italic t         )) (((class color)) (:foreground "FireBrick"       ))))
 '(font-lock-constant-face      ((((class color) (background dark)) (:foreground "#00EA75"                  )) (((class color)) (:foreground "ForestGreen"     )))) 
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "cyan"                     )) (((class color)) (:foreground "purple"          ))))
 '(font-lock-doc-string-face    ((((class color) (background dark)) (:foreground "#00FA85" :bold t          )) (((class color)) (:foreground "darkGreen"       ))))
 '(font-lock-keyword-face       ((((class color) (background dark)) (:foreground "#87CEFA"                  )) (((class color)) (:foreground "SkyBlue" :bold t )))) 
 '(font-lock-preprocessor-face  ((((class color) (background dark)) (:foreground "#87CEFA" :bold t          )) (((class color)) (:foreground "gray40"          ))))
 '(font-lock-reference-face     ((((class color) (background dark)) (:foreground "wheat40" :bold t          )) (((class color)) (:foreground "maroon4"         ))))
 '(font-lock-string-face        ((((class color) (background dark)) (:foreground "#00EA75"                  )) (((class color)) (:foreground "ForestGreen"     ))))
 '(font-lock-type-face          ((((class color) (background dark)) (:foreground "wheat40" :bold t          )) (((class color)) (:foreground "maroon4"         ))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "yellow"                   )) (((class color)) (:foreground "SaddleBrown"     ))))
 '(font-lock-warning-name-face  ((((class color) (background dark)) (:foreground "DarkOrange"               )) (((class color)) (:foreground "DarkOrange"      ))))
 '(paren-face-match             ((((class color))                   (:background "green" :foreground "black"))                                                   ))
 '(paren-face-mismatch          ((((class color))                   (:background "black" :foreground "red"  ))                                                   ))
 '(paren-face-no-match          ((((class color))                   (:background "red"   :foreground "black"))                                                   ))
 '(show-paren-match             ((((class color))                   (:background "green2"                   ))                                                   ))
 '(show-paren-mismatch          ((((class color))                   (:background "red"   :foreground "white"))                                                   ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake-mode
;para python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
       (local-file (file-relative-name
                    temp-file
                    (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
           '("\\.py\\'" flymake-pylint-init)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; perl replace
(defun perl-replace (start end)
  "Replace a text pattern in a  region using perl expressions"
  (interactive "*r")
  (setq regexp (read-string "Regexp: "))
  (setq to-string (read-string (concat "[" regexp "] Replacement: ")))
  (setq command (concat "perl -e 's/" regexp "/" to-string "/g' -p"))
  (shell-command-on-region start end command nil 1)
  )
(global-set-key "\M-r" 'perl-replace)
