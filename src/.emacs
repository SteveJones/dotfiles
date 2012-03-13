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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'org)

(setq tramp-default-method "scpc")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-mobile-inbox-for-pull "~/org/flagged")
(setq org-mobile-directory "/mnt/hgfs/stephenjones/Dropbox/org")

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies 't)

(defun generate-file-template ()
  (let ((file-name (buffer-file-name))
	(line-number (line-number-at-pos)))
    (concat "* " file-name ":" (int-to-string line-number) " %T")))

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

(setq minibuffer-frame-alist '((width . 80) (height . 3)))

(defun my-org-agenda-format-date-aligned (date)
  "Format a date string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
    (format "%-10s %2d %-10s %4d%s"
	    dayname day monthname year weekstring)))


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
  (compilation-start (concat "ack-grep -H --nogroup --color --column --" type " '" query "' '" directory "'") 'grep-mode))

(defvar ack-history nil)

(defun get-symbol-prompt (prompt)
  (let ((default (thing-at-point 'symbol)))
    (read-string
     (if default
	 (concat prompt " (default " default "): ")
       (concat prompt ": "))
     nil
     'ack-history
     default)))

(defun ack ()
  (interactive)
  (let ((query (get-symbol-prompt "Ack")))
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

(global-set-key (kbd "C-c g") 'w3m-search)
(global-set-key (kbd "C-c S-g") (lambda () (w3m-search "google")))

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode)
  (hs-minor-mode)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (setq ack-type "elisp")
  (make-local-variable 'w3m-search-default-engine)
  (setq w3m-search-default-engine "emacswiki"))

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
  (setq ack-type "python")
  (add-hook 'local-write-file-hooks
	    '(lambda()
	       (save-excursion
		 (delete-trailing-whitespace)))))

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

(defun url-safe-p (c)
  "True if the character is safe to use in a url without encoding."
  (and (and (> c 32) (< c 128))
       (not (member c '(?: ?/ ?? ?# ?[ ?] ?@ ?! ?$ ?& ?' ?( ?) ?* ?+ ?, ?\; ?=)))))

(defun url-encode-char (c)
  "Url encodes a single character, returns a string."
  (cond ((= ?  c) "+")
	((url-safe-p c) (char-to-string c))
	('t (format "%%%02x" c))))

(defun url-encode (string)
  "Returns the url encoded value of its argument."
  (mapconcat 'url-encode-char (encode-coding-string string 'utf-8) ""))

(defun postgres-w3m-find-results (url)
  (if (string-prefix-p "http://www.postgresql.org/search/" url)
      (search-forward "1.")
    (recenter-top-bottom 3)))

(add-hook 'w3m-display-hook 'postgres-w3m-find-results)

(defun postgres-get-help (symbol)
  (w3m (concat
	"http://www.postgresql.org/search/?u=%2Fdocs%2F9.1%2F&q="
	(url-encode symbol))))

(defvar postgres-help-history nil)

(defun postgres-help ()
  (interactive)
  (let ((query (get-symbol-prompt "Postgres docs")))
    (unless (string= query "")
      (add-to-list 'postgres-help-history query)
      (postgres-get-help query))))

(defun my-sql-mode-hook ()
  (local-set-key (kbd "C-c f") 'postgres-help)
  (setq ack-type "sql"))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)

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

(setq gnus-select-method 
      '(nnimap "mail.secretvolcanobase.org"
	       (nnimap-address "mail.secretvolcanobase.org")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(require 'smtpmail)
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-skip-threshold 1)
 '(compilation-window-height 20)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".orig")))
 '(confluence-default-space-alist (quote (("https://hq.hanzoarchives.com/confluence/rpc/xmlrpc" . "dashboard"))))
 '(confluence-url "https://hq.hanzoarchives.com/confluence/rpc/xmlrpc")
 '(dvc-confirm-add nil)
 '(dvc-tips-enabled nil)
 '(fill-column 78)
 '(gnus-ignored-newsgroups "")
 '(gnus-select-method (quote (nnimap "mail.secretvolcanobase.org")))
 '(gnus-summary-line-format "%U%R%z%>%(%[%-23,23f%]%) %s
")
 '(hs-hide-comments-when-hiding-all nil)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-auto-rescan t)
 '(ispell-dictionary "british")
 '(ispell-local-dictionary "british")
 '(jira-url "https://hq.hanzoarchives.com/jira/rpc/xmlrpc")
 '(mm-text-html-renderer (quote w3m))
 '(mm-verify-option (quote always))
 '(org-agenda-custom-commands (quote (("h" "Agenda + household TODOs" ((agenda "" nil) (tags-todo "@home|garden|bill" nil)) nil) ("T" "In Town" ((agenda "" ((org-agenda-span (quote day)))) (tags-todo "@town|shopping" nil)) nil))))
 '(org-agenda-deadline-faces (quote ((1.0 . org-warning) (0.5 . org-upcoming-deadline) (0.0 . default))))
 '(org-agenda-diary-file "~/org/diary.org")
 '(org-agenda-files (quote ("~/org/notes.org" "~/org/personal.org" "~/org/hanzo.org" "~/org/diary.org" "~/org/shopping.org")))
 '(org-agenda-format-date (quote my-org-agenda-format-date-aligned))
 '(org-agenda-menu-show-matcher t)
 '(org-agenda-menu-two-column nil)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-deadlines (quote near))
 '(org-agenda-todo-ignore-scheduled (quote all))
 '(org-agenda-todo-ignore-timestamp nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t) (awk . t) (sh . t) (R . t) (sql . t))))
 '(org-capture-templates (quote (("e" "Event" entry (file "~/org/diary.org") "* %?
  :PROPERTIES:
  :created: %U
  :END:") ("s" "Shopping list item" entry (file+headline "~/org/shopping.org" "Shopping list") "* NEED %? :shopping:
 :PROPERTIES:
 :created: %U
 :END:") ("f" "Note about current file" entry (file+headline "~/org/notes.org" "Code notes") (function generate-file-template)) ("n" "Note" entry (file+headline "~/org/notes.org" "Notes") "* %?
 :PROPERTIES:
 :created: %U
 :END:") ("w" "Work Task" entry (file+headline "~/org/notes.org" "Tasks") "* TODO %?
 DEADLINE: %^{DEADLINE}T
 :PROPERTIES:
 :created: %U
 :END:
%^{report to}p") ("t" "Task" entry (file+headline "~/org/notes.org" "Tasks") "* TODO %?
 DEADLINE: %^T
 :PROPERTIES:
 :created: %U
 :END:"))))
 '(org-default-notes-file "~/org/notes.el")
 '(org-mobile-inbox-for-pull "~/org/from-mobile.org" t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m org-choose org-collector org-drill org-git-link org-jira orgtbl-sqlinsert)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 10))))
 '(org-src-fontify-natively t)
 '(org-tag-persistent-alist (quote ((:startgroup) ("@code" . 99) ("@home" . 104) ("@town" . 116) (:endgroup) ("bill" . 98) ("shopping" . 115))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(read-mail-command (quote gnus))
 '(remember-data-file "~/remember.org")
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(smtpmail-debug-info t)
 '(smtpmail-local-domain nil)
 '(smtpmail-smtp-server "mail.secretvolcanobase.org")
 '(smtpmail-smtp-service "smtp")
 '(smtpmail-smtp-user "zombywuf")
 '(smtpmail-stream-type (quote starttls))
 '(user-mail-address "steve@secretvolcanobase.org")
 '(vc-delete-logbuf-window nil)
 '(w3m-search-engine-alist (quote (("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil) ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan) ("alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8) ("blog" "http://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8) ("blog-en" "http://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8) ("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8) ("google-en" "http://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8) ("google news" "http://news.google.co.jp/news?hl=ja&ie=utf-8&q=%s&oe=utf-8" utf-8) ("google news-en" "http://news.google.com/news?hl=en&q=%s" nil) ("google groups" "http://groups.google.com/groups?q=%s" nil) ("All the Web" "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s" nil) ("All the Web-ja" "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp&q=%s" euc-japan) ("technorati" "http://www.technorati.com/search/%s" utf-8) ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8) ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8) ("goo-ja" "http://search.goo.ne.jp/web.jsp?MT=%s" euc-japan) ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis) ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil) ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil) ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil) ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil) ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan) ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice" nil) ("waei" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan) ("eiwa" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej" nil) ("kokugo" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan) ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67" nil) ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s") ("amazon-ja" "http://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis) ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil) ("en.wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil) ("de.wikipedia" "http://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8) ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8) ("msdn" "http://search.msdn.microsoft.com/search/default.aspx?query=%s" nil) ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil) ("postgres" "http://www.postgresql.org/search/?u=%%2Fdocs%%2F9.1%%2F&q=%s" utf-8)))))
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
 '(gnus-header-content ((t (:foreground "indianred4"))))
 '(italic ((((supports :underline t)) (:slant italic))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :height 1.5))) t)
 '(org-agenda-date-today ((t (:inherit org-agenda-date :background "gray90" :slant italic :weight bold))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "grey20"))) t)
 '(org-agenda-done ((t (:foreground "dark green"))))
 '(org-agenda-structure ((t (:foreground "dark blue" :box (:line-width 8 :color "white") :underline t))))
 '(org-block-background ((t (:background "grey90"))))
 '(org-done ((t (:background "ForestGreen" :foreground "white" :box (:line-width 2 :color "darkgreen" :style released-button) :weight bold))))
 '(org-scheduled-today ((t (:foreground "Green4" :weight bold))))
 '(org-table ((t (:foreground "grey20" :box (:line-width 1 :color "grey75")))))
 '(org-tag ((t (:box (:line-width 2 :color "blue") :weight bold))))
 '(org-todo ((t (:background "red1" :foreground "white" :box (:line-width 2 :color "red4" :style released-button) :weight bold))))
 '(org-upcoming-deadline ((t (:inherit default :weight bold))))
 '(outline-1 ((t (:foreground "medium blue" :weight bold :height 1.3))))
 '(outline-2 ((t (:foreground "medium blue" :weight bold :height 1.1))))
 '(outline-3 ((t (:foreground "medium blue" :weight bold))))
 '(outline-4 ((t (:background "gray90" :foreground "forest green" :weight bold))))
 '(outline-5 ((t (:background "gray93" :foreground "forest green" :weight bold))))
 '(outline-6 ((t (:background "gray96" :foreground "forest green" :weight bold))))
 '(outline-7 ((t (:background "gray99" :foreground "forest green" :weight bold)))))

(put 'narrow-to-region 'disabled nil)
