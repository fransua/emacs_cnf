
(setq frame-title-format "%f")
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq load-path (cons "~/.emacs.d/auto-complete" load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; python
; apt-get install ipython
; apt-get install pylint
; pylint --generate-rcfile > ~/.pylintrc #warnings...
; apt-get install python-mode

; if file ends with .py -> python mode
(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq interpreter-mode-alist(cons '("python" . python-mode)
				  interpreter-mode-alist))

(add-hook 'python-mode-hook 'my-python-hook)

;; this getz called after python mode is enabled
(defun my-python-hook ()
  (require 'ipython)
  (require 'auto-complete)
  (global-auto-complete-mode +1)
  (set-cursor-color "white")
  ;; Alt + arrows indent
  (local-set-key [\M-\right] 'py-shift-region-right)
  (local-set-key [\M-\left]  'py-shift-region-left)
  ;; ipython completion
  (local-set-key [\s-\tab]   'ipython-complete)
  (define-key py-mode-map "\C-c\C-R"     'py-execute-region)
  (show-paren-mode 1)
  (custom-set-variables
   '(py-shell-switch-buffers-on-execute nil) ; no switching for ipython
   '(py-shell-initial-switch-buffers nil)
   )
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Flymake-mode
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (when (load "flymake" t)
    (defun flymake-pylint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "epylint" (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
		 '("\\.py\\'" flymake-pylint-init))
    )
  ;;To avoid having to mouse hover for the error message, these functions make flymake error messages
  ;;appear in the minibuffer
  ;; not working with ipython - completion trick use \M-s key to see message
  ;; (defun show-fly-err-at-point ()
  ;;   "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  ;;   (interactive)
  ;;   (let ((line-no (line-number-at-pos)))
  ;;     (dolist (elem flymake-err-info)
  ;;       (if (eq (car elem) line-no)
  ;; 	  (let ((err (car (second elem))))
  ;; 	    (message "%s" (flymake-ler-text err)))))))
  ;; (add-hook 'post-command-hook 'show-fly-err-at-point)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; latex
(require 'tex-site)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq TeX-auto-save t
      TeX-newline-function 'reindent-then-newline-and-indent
      TeX-parse-self t
      LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title
	LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label))
;(global-auto-revert-mode t)
;(add-hook 'LaTeX-mode-hook 'flymake-mode)

;; autocomplete for math to
;(require 'ac-math)
;(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
;(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;  (setq ac-sources
;     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;               ac-sources))
;)
;(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
;(ac-flyspell-workaround)


(setq ispell-program-name "aspell")
    ; could be ispell as well, depending on your preferences 
(setq ispell-dictionary "english")
    ; this can obviously be set to any language your spell-checking program supports
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;; to open evince at current page
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "firefox %o"))))


;templates
(require 'template)
(template-initialize)

;; moving arround
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<left>") 'windmove-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; general
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(pc-selection-mode t nil (pc-select))
 '(scroll-bar-mode nil)
 '(setq visible-bell t)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t))

; key-bindings
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'display-local-help)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "cyan"))))
 '(font-lock-comment-delimiter-face ((default nil) (((class color) (min-colors 16)) (:foreground "darkred"))))
 '(font-lock-comment-face ((t (:foreground "brown" :slant italic))))
 '(font-lock-doc-string-face ((((class color) (background dark)) (:foreground "#00FA85" :bold t)) ((class color) (:foreground "darkGreen"))))
 '(font-lock-function-name-face ((nil (:foreground "palegreen" :weight bold))))
 '(font-lock-keyword-face ((((class color) (min-colors 8)) (:foreground "cyan3" :weight bold))))
 '(font-lock-reference-face ((((class color) (background dark)) (:foreground "#FAD987" :bold t)) ((class color) (:foreground "maroon4"))))
 '(font-lock-string-face ((nil (:foreground "green3"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen" :weight bold))))
 '(paren-face-match ((((class color)) (:background "green" :foreground "black"))))
 '(paren-face-mismatch ((((class color)) (:background "black" :foreground "red"))))
 '(paren-face-no-match ((((class color)) (:background "red" :foreground "grey"))))
 '(py-builtins-face ((t (:foreground "violet" :weight normal))) t)
 '(py-pseudo-keyword-face ((t (:foreground "gray50" :weight normal))) t)
 '(show-paren-match ((((class color)) (:background "green2"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))


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

(put 'upcase-region 'disabled nil)
