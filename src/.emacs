(require 'ido)
(require 'imenu)
(require 'cl)
(require 'tramp)
(if (file-exists-p "~/code/emacsweblogs/lisp")
    (progn
      (add-to-list 'load-path "~/code/emacsweblogs/lisp")
      (require 'xml-rpc)
      (if (file-exists-p "~/code/jira-mode")
	  (progn
	    (add-to-list 'load-path "~/code/jira-mode")
	    (require 'jira)))))
(require 'epa-file)
(epa-file-enable)
(if (file-exists-p "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el")
    (load-file "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el"))

(if (file-exists-p "~/code/confluence-el-read-only")
    (progn
      (add-to-list 'load-path "~/code/confluence-el-read-only")
      (require 'confluence)))
(require 'org)
(require 'remember)

(setq tramp-default-method "scpc")

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-default-notes-file (concat org-directory "notes.el"))
(global-set-key (kbd "C-c c") 'org-remember)

(defun de-camel ()
  """CamelCaseIsEvilAndMustBeDestroyed"""
  (interactive)
  (while (looking-at "\\w")
    (let ((case-fold-search nil)
	  (c (char-after)))
      (if (char-equal (upcase c) c)
	  (progn
	    (insert "_" (downcase (char-after)))
	    (delete-char 1))
	(forward-char 1)))))

(global-set-key (kbd "C-x C-h") 'de-camel)

(setq tester-file-patterns
  (list
   (list "\\.py$"
	 '("unit2" "discover" "-p" "*_tests.py")
	 '(("^File" "^\\*\\{70\\}$")
	   ("^File \"\\([^\"]+\\)\", line \\([[:digit:]]+\\)" '((1 . file) (2 . lineno)))
	   ("^\\(Exception raised:\n\\(.\\|\n\\)*\\)\n\\*\\{70\\}$" '((1 . message)))))))

(defvar tester-file-patterns
  (list
   (list "\\.py$"
	 '("unit2" "discover" "-p" "*_tests.py")
	 '(("^File" "^\\*\\{70\\}$")
	   ("^File \"\\([^\"]+\\)\", line \\([[:digit:]]+\\)" '((1 . file) (2 . lineno)))
	   ("^\\(Exception raised:\n\\(.\\|\n\\)*\\)\n\\*\\{70\\}$" '((1 . message)))))))

(defun list-find (l pred)
  (if l
      (if (funcall pred (car l))
	  (car l)
	(list-find (cdr l) pred))
    nil))
  
(defun tester-find-appropriate (filename)
  (cdr (list-find tester-file-patterns
		  (lambda (test)
		    (string-match (car test) filename)))))

(make-face 'tester-highlight-face)
(set-face-background 'tester-highlight-face "thistle")

(setq debug-on-error t)
;; TODO
(defun tester-run-tests ()
  (interactive)
  (let* ((buffer (current-buffer))
	 (test (tester-find-appropriate (buffer-file-name buffer)))
	 (output (generate-new-buffer "*Tester*"))
	 (run-process (append '(start-process "tester" output)
			      (car test))))
    (eval run-process)
    
    ))

;; Lifted from http://nflath.com/2009/09/emacs-fixes/
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
		       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist)
      (let* ((symbol-at-point (symbol-name (symbol-at-point)))
	     (selected-symbol (ido-completing-read
			       "Symbol? "
			       (if (member symbol-at-point symbol-names)
				   (cons symbol-at-point
					 (remove-if (lambda (x) (string-equal x symbol-at-point))
						    symbol-names))
				 symbol-names)))
	     (pos (cdr (assoc selected-symbol name-and-pos))))
	(if (overlayp pos)
	    (goto-char (overlay-start pos))
	  (goto-char pos))))))

(global-set-key (kbd "C-S-s") 'ido-goto-symbol)

(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
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

(defun project-root ()
  (let* ((path (buffer-file-name))
	 (parts (split-string path "/"))
	 (root ""))
    (while (and parts (not (string= (car parts) "code")))
      (unless (string= "" (car parts))
	(setq root (concat root "/" (car parts))))
      (setq parts (cdr parts)))
    ;; Adds "code"
    (if (string= (car parts) "code")
	(progn
	  (setq root (concat root "/" (car parts)))
	  (setq parts (cdr parts))
	  ;; Adds directory above code
	  (if parts
	      (setq root (concat root "/" (car parts))))))
    (if (not (file-directory-p root))
	(file-name-directory root)
      root)))

(defvar ack-type "all"
  "The type of file to search with the ack function. This should
be made buffer local and set to the file type in load hooks.")

(make-variable-buffer-local 'ack-type)

(defun run-ack (type query directory)
  (compilation-start (concat "ack-grep -H --nogroup --nocolor --" type " '" query "' '" directory "'") 'grep-mode))

(defvar ack-history nil)

(defun ack-get-query (prompt)
  (let ((default (thing-at-point 'symbol)))
    (read-string
     (concat prompt " (default " default "): ")
     nil
     'ack-history
     default)))

(defun ack ()
  (interactive)
  (let ((query (ack-get-query "Ack")))
    (if (string= query "")
	()
      (add-to-list 'ack-history query)
      (run-ack ack-type query (project-root)))))

(defun ack-all ()
  (interactive)
  (let ((query (ack-get-query "Ack all")))
    (if (string= query "")
	()
      (add-to-list 'ack-history query)
      (run-ack "all" query (project-root)))))

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode)
  (hs-minor-mode)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (setq ack-type "elisp"))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(when (load "flymake" t)
  ;; Python
  (setq flymake-log-level 2)

  (defun flymake-create-temp-intemp (file-name prefix)
    "Return file name in temporary directory for checking
     FILE-NAME. This is a replacement for
     `flymake-create-temp-inplace'. The difference is that it
     gives a file name in `temporary-file-directory' instead of
     the same directory as FILE-NAME.

     For the use of PREFIX see that function.

     Note that not making the temporary file in another directory
     \(like here) will not if the file you are checking depends
     on relative paths to other files \(for the type of checks
     flymake makes)."
    (unless (stringp file-name)
      (error "Invalid file-name"))
    (or prefix
	(setq prefix "flymake"))
    (let* ((name (concat
		  (file-name-nondirectory
		   (file-name-sans-extension file-name))
		  "_" prefix))
	   (ext  (concat "." (file-name-extension file-name)))
	   (temp-name (make-temp-file name nil ext))
	   )
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-intemp))
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
  (local-set-key (kbd "C-c f") 'python-describe-symbol)
  (setq ack-type "python"))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun cperl-describe-symbol ()
  (interactive)
  (cperl-describe-perl-symbol))

(defun my-perl-mode-hook ()
  (flymake-mode)
  (local-set-key (kbd "C-c f") 'cperl-describe-symbol)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (setq ack-type "perl"))

(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(defun my-c-mode-hook ()
  (flymake-mode)
  (hs-minor-mode)
  (local-set-key (kbd "C-c f") 'manual-entry)
  (setq ack-type "cc"))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (flymake-mode)
  (hs-minor-mode)
  (local-set-key (kbd "C-c f") 'manual-entry)
  (setq ack-type "cpp"))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'compilation-error-regexp-alist-alist
	     '(boost-test-failure "^\\([^(]+\\)(\\([[:digit:]]+\\)):\\s-+fatal\\s-+error"))

(add-to-list 'compilation-error-regexp-alist 'boost-test-failure)

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
 '(compilation-window-height 20)
 '(confluence-default-space-alist (quote (("https://hq.hanzoarchives.com/confluence/rpc/xmlrpc" . "dashboard"))))
 '(confluence-url "https://hq.hanzoarchives.com/confluence/rpc/xmlrpc")
 '(hs-hide-comments-when-hiding-all nil)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ispell-dictionary "british")
 '(ispell-local-dictionary "british")
 '(jira-url "https://hq.hanzoarchives.com/jira/rpc/xmlrpc")
 '(org-agenda-files (quote ("~/notes.org")))
 '(org-todo-keywords (quote ((sequence "TODO" "BUG_CREATED" "IN_PROGRESS" "DONE"))))
 '(read-mail-command (quote gnus))
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
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:foreground "blue1"))))
 '(font-lock-comment-delimiter-face ((default (:foreground "Firebrick4")) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "brown"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "salmon4"))))
 '(font-lock-doc-face ((t (:foreground "darkslategrey"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue3"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "Purple3"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "darkgreen"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "dark slate grey"))))
 '(italic ((((supports :underline t)) (:slant italic)))))

(put 'narrow-to-region 'disabled nil)
