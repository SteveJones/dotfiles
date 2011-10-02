(require 'ido)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setq inhibit-startup-screen t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)

(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg. For use in hooks."
   (interactive)
   (flyspell-mode 1))

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'tcl-mode-hook 'flyspell-prog-mode)
(add-hook 'sql-mode-hook 'flyspell-prog-mode)

(defvar ack-type "all"
  "The type of file to search with the ack function. This should
be made buffer local and set to the file type in load hooks.")

(defun run-ack (type query)
  (compilation-start (concat "ack-grep -H --nogroup --nocolor --" type " " query) 'grep-mode))

(defvar ack-history nil)

(defun ack ()
  (interactive)
  (let ((query (read-string "Ack: " nil ack-history)))
    (if (string= query "")
	()
      (add-to-list 'ack-history query)
      (run-ack ack-type query))))

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode)
  (hs-minor-mode)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(when (load "flymake" t)
  ;; Python
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  ;; Perl
  (defun flymake-perl-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "perl" (list "-wc " local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
	       '(".+\\.[pP][lLmM]$"
		 flymake-perl-init
		 flymake-simple-cleanup
		 flymake-get-real-file-name))

  (add-to-list 'flymake-err-line-patterns
	       '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defun my-python-mode-hook ()
  (setq insert-tabs-mode '())
  (flymake-mode)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (local-set-key (kbd "C-c f") 'python-describe-symbol))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun cperl-describe-symbol ()
  (interactive)
  (cperl-describe-perl-symbol))

(defun my-perl-mode-hook ()
  (flymake-mode)
  (local-set-key (kbd "C-c f") 'cperl-describe-symbol)
  (hs-minor-mode)
  (flyspell-prog-mode))

(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(defun my-c-mode-hook ()
  (flymake-mode)
  (hs-minor-mode)
  (local-set-key (kbd "C-c f") 'man-follow))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;(setq compilation-error-regexp-alist-alist (assq-delete-all 'miyamoto-error compilation-error-regexp-alist-alist))
;(setq compilation-error-regexp-alist-alist (assq-delete-all 'miyamoto-info compilation-error-regexp-alist-alist))

(add-to-list 'compilation-error-regexp-alist-alist
	     '(miyamoto-error "^[[:digit:]-T:]\\{19\\}Z\\s-+ERROR\\s-\\([^:]+\\):\\([[:digit:]]+\\)"
			      1 2 nil 2))
(add-to-list 'compilation-error-regexp-alist-alist
	     '(miyamoto-info "^[[:digit:]-T:]\\{19\\}Z\\s-+\\(INFO\\|DEBUG\\)\\s-\\([^:]+\\):\\([[:digit:]]+\\)"
			     2 3 nil 0))
(add-to-list 'compilation-error-regexp-alist 'miyamoto-error)
(add-to-list 'compilation-error-regexp-alist 'miyamoto-info)

(defun miyamoto-test ()
  (interactive)
  (let ((display (getenv "DISPLAY"))
	(miyamoto_absolute_path (getenv "MIYAMOTO_ABSOLUTE_PATH")))
    (setenv "DISPLAY" ":6")
    (setenv "MIYAMOTO_ABSOLUTE_PATH" "true")
    (compile (concat "python " buffer-file-name))
    (setenv "DISPLAY" display)
    (setenv "MIYAMOTO_ABSOLUTE_PATH" miyamoto_absolute_path)))

;(setq compilation-error-regexp-alist-alist (assq-delete-all 'psql-error compilation-error-regexp-alist-alist))
;(setq compilation-error-regexp-alist-alist (assq-delete-all 'psql-info compilation-error-regexp-alist-alist))

(add-to-list 'compilation-error-regexp-alist-alist
	     '(psql-notice "^psql:\\([^:]+\\):\\([[:digit:]]+\\):\\s-+NOTICE" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist-alist
	     '(psql-error "^psql:\\([^:]+\\):\\([[:digit:]]+\\):\\s-+ERROR" 1 2 nil 2))
(add-to-list 'compilation-error-regexp-alist 'psql-notice)
(add-to-list 'compilation-error-regexp-alist 'psql-error)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(compilation-skip-threshold 1)
 '(compilation-window-height 10)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(vc-delete-logbuf-window nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(flymake-errline ((((class color) (background light)) (:underline "red"))))
 '(flymake-warnline ((((class color) (background light)) (:underline "steelblue"))))
 '(flyspell-duplicate ((t (:underline "gold4"))))
 '(flyspell-incorrect ((t (:underline "red"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:foreground "steelblue4"))))
 '(font-lock-comment-delimiter-face ((default (:foreground "Firebrick4")) (((class color) (min-colors 16)) nil)))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "orangered"))))
 '(font-lock-doc-face ((t (:foreground "brown2"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "Brown"))))
 '(italic ((((supports :underline t)) (:slant italic)))))
