
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq frame-title-format "%f")
(setq load-path (cons "~/.emacs.d/lisp"			 load-path))
(setq load-path (cons "~/.emacs.d/lisp/epc"		 load-path))
(setq load-path (cons "~/.emacs.d/lisp/ctable"		 load-path))
(setq load-path (cons "~/.emacs.d/lisp/deferred"	 load-path))
(setq load-path (cons "~/.emacs.d/lisp/auto-complete"	 load-path))
(setq load-path (cons "~/.emacs.d/lisp/emacs-jedi"	 load-path))
(setq load-path (cons "~/.emacs.d/lisp/highlight-indent" load-path))
(setq load-path (cons "~/.emacs.d/lisp/autopair"	 load-path))
(setq load-path (cons "~/.emacs.d/lisp/popup-el"	 load-path))
(setq load-path (cons "~/.emacs.d/lisp/emacs-helm"	 load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
      (set-frame-size (selected-frame) 165 70))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL NOTES
; python
; apt-get install ipython
; apt-get install pylint
; pylint --generate-rcfile > ~/.pylintrc #warnings...
; apt-get install python-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what to do depending on file extension
(setq auto-mode-alist
      (append '(
                ("\\.css\\'" . css-mode)
                ("\\.\\(htm\\|html\\|xhtml\\)$" . html-mode)
                ("\\.sql$" . sql-mode)
                ("\\.js$" . js-mode)
                ("\\.json$" . js-mode)
                ("\\.js$" . js-mode)
                ("\\.py" . python-mode)
		("\\.gp" . gp-mode)
                ;; sorted by chapter
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
                ("\\.txt$" . org-mode)
                ("\\.dat$" . ledger-mode)

                ("\\.log$" . text-mode)
                ("\\.tex$" . LaTeX-mode)
                ("\\.tpl$" . LaTeX-mode)
                ("\\.cgi$" . perl-mode)
                ("[mM]akefile" . makefile-mode)
                ("\\.bash$" . shell-script-mode)
                ("\\.expect$" . tcl-mode)

                (".ssh/config\\'" . ssh-config-mode)
                ("sshd?_config\\'" . ssh-config-mode)
                ) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON!
;; function declarations:
(declare-function global-auto-complete-mode            "auto-complete.el")
(declare-function flymake-init-create-temp-buffer-copy "flymake.el"      )

;; run hook
(add-hook 'python-mode-hook 'my-python-hook)

;; autopair hook for triple quotes
(add-hook 'python-mode-hook
	  #'(lambda ()
	      (setq autopair-handle-action-fns
		    (list #'autopair-default-handle-action
			  #'autopair-python-triple-quote-action))))

;; this is called after python mode is enabled
(defun my-python-hook ()
  (require 'helm-config)
  (require 'helm)
  (require 'anything)
  (defvar py-mode-map)
  (require 'ipython)

  ;; the little line
  ;; function to remove the little line when completing, 
  ;; and also to set visual line mode
  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode))
    (when (not visual-line-mode)
      (visual-line-mode)))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode))
    (when (not visual-line-mode)
      (visual-line-mode)))
  ;; the little line
  (require 'fill-column-indicator)
  (fci-mode)
  (setq fci-rule-column 80)
  (setq fci-rule-width 3)
  (setq fci-rule-color "grey25")

  ;; highlight columns
  (require 'highlight-indentation)
  (highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#0E0E0E")

  ;; autopair
  (require 'autopair)
  (autopair-global-mode)

  ;; shortcuts enabdl C-c d / C-. (must be before the call of jedi)
  (setq jedi:setup-keys t) ; keys
  ;;(setq jedi:tooltip-method nil) ; tool-tip popup in minibuffer
  (eval-when-compile (require 'jedi nil t))
  (require 'jedi)
  (require 'auto-complete)
  (autoload 'jedi:setup "jedi" t t)
  (global-auto-complete-mode +1)

  ;; just in order to keep on the right side of the force
  (set-cursor-color "white")

  ;; opening bracket doc
  (jedi-mode 1)
  (add-hook 'python-mode-hook 'jedi:setup)

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
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates
(require 'template)
(template-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moving arround
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<left>") 'windmove-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIDE/SHOW code folding
;; Code folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)

(global-set-key (kbd "<C-s-down>") 'hs-show-all)
(global-set-key (kbd "<C-s-up>") 'hs-hide-all)
(global-set-key (kbd "<C-s-right>") 'hs-toggle-hiding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the following to your .emacs and uncomment it in order to get a + symbol
;; in the fringe and a yellow marker indicating the number of hidden lines at
;; the end of the line for hidden regions:
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)
(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)
(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)
(defface hs-face
  '((t (:background "grey2" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format " %d lines...\n" (count-lines (overlay-start ov) (overlay-end ov))))
           )
      (overlay-put ov 'help-echo "Hiddent text. C-+ to show, C-M-+ show all, C-s-+ hide all")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))
(setq hs-set-up-overlay 'display-code-line-counts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(org-agenda-files (quote ("~/Ubuntu One/org-things/first.org")))
 '(pc-selection-mode t nil (pc-select))
 '(scroll-bar-mode nil)
 '(set-cursor-color "white")
 '(setq visible-bell t)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; key-bindings
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'display-local-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACES
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; perl replace
(defun perl-replace (start end)
  "Replace a text pattern in a  region using perl expressions"
  (interactive "*r")
  (defvar regexp)
  (defvar to-string)
  (defvar command)
  (setq regexp (read-string "Regexp: "))
  (setq to-string (read-string (concat "[" regexp "] Replacement: ")))
  (setq command (concat "perl -e 's/" regexp "/" to-string "/g' -p"))
  (shell-command-on-region start end command nil 1)
  )
(global-set-key "\M-r" 'perl-replace)

(put 'upcase-region 'disabled nil)
