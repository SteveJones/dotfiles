(require 'ido)
(require 'imenu)
(require 'cl)
(require 'epa-file)
(epa-file-enable)

(if (file-exists-p "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el")
    (load-file "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el"))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(require 'org)
		  
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq tramp-default-method "scpc")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(require 'w3m-load)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
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


(defun prefixes (of-list)
  (if of-list
      (cons (list (car of-list))
	    (mapcar
	     (lambda (p) (cons (car of-list) p))
	     (prefixes (cdr of-list))))))

(defun scoped-symbol-at-point ()
  (let ((func (which-function)))
    (if func
	(let* ((scope (split-string func "\\."))
	       (symbol (symbol-name (symbol-at-point)))) 
	  (if (not (symbol-at-point))
	      '()
	    (message symbol)
	    (cons symbol
		  (mapcar
		   (lambda (prefix)
		     (mapconcat 'identity (append prefix (list symbol)) "."))
		   (prefixes scope))))))))

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
      (let* ((scoped-symbols (remove-if
			      (lambda (name) (not (member name symbol-names)))
			      (scoped-symbol-at-point)))
	     (symbol-at-point (if scoped-symbols
				   (car scoped-symbols)))
	     (selected-symbol (ido-completing-read
			       "Symbol? "
			       (if (member symbol-at-point symbol-names)
				   (cons symbol-at-point
					 (remove-if (lambda (x) (string-equal x symbol-at-point))
						    symbol-names))
				 symbol-names)))
	     (pos (cdr (assoc selected-symbol name-and-pos))))
	(push-mark)
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

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-line
					 try-expand-line-all-buffers
					 try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)

(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg. For use in hooks."
   (interactive)
   (flyspell-mode 1))

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'tcl-mode-hook 'flyspell-prog-mode)

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
  (compilation-start (concat "ack-grep -i -H --nogroup --color --column --" type " '" query "' '" directory "'") 'grep-mode))

(defvar ack-history nil)

(defun get-symbol-prompt (prompt history)
  (let* ((default (thing-at-point 'symbol))
	 (string (read-string
		  (if default
		      (concat prompt " (default " default "): ")
		    (concat prompt ": "))
		  nil
		  history
		  default)))
    (unless (string= string "")
	(add-to-list history string))
    string))

(defun ack ()
  (interactive)
  (let ((query (get-symbol-prompt "Ack" 'ack-history)))
    (unless (string= query "")
      (run-ack ack-type query (project-root)))))

(defun ack-all ()
  (interactive)
  (let ((query (ack-get-query "Ack all")))
    (if (string= query "")
	()
      (add-to-list 'ack-history query)
      (run-ack "all" query (project-root)))))

(global-set-key (kbd "C-x C-a") 'ack)

(defvar w3m-search-history nil)

(defun w3m-search-at-point ()
  "Do a web search with a default query of the symbol at the
point"
  (interactive)
  (w3m-search
   w3m-search-default-engine
   (get-symbol-prompt
    (concat "Search " w3m-search-default-engine)
    'w3m-search-history)))

(defun w3m-google-at-point ()
  "Do a google search with a default query of the symbol at the point."
  (interactive)
  (w3m-search
   "google"
   (get-symbol-prompt "Search Google" 'w3m-search-history)))
  

(global-set-key (kbd "C-c g") 'w3m-search-at-point)
(global-set-key (kbd "C-c C-g") 'w3m-google-at-point)

(defun my-emacs-lisp-mode-hook ()
  (flyspell-prog-mode)
  (hs-minor-mode)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially)
  (setq ack-type "elisp")
  (make-local-variable 'w3m-search-default-engine)
  (setq w3m-search-default-engine "emacswiki"))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(when (load "flymake" t)
  ;; Python
  (setq flymake-log-level 3)

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
	   (temp-name (make-temp-file name nil ext)))
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-intemp))
	   (local-dir (file-name-directory buffer-file-name))
           (local-file (file-relative-name
                        temp-file
                        local-dir)))
      
      (list "epylint" (list local-file) local-dir)))
  
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
	       '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1))

  (defun flymake-d-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "dmd" (list "-unittest" "-c" "-wi" local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '(".+\\.d$"
		 flymake-d-init
		 flymake-simple-cleanup
		 flymake-get-real-file-name))

  (add-to-list 'flymake-err-line-patterns
	       '("\\(.*\\)(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3)))

(defun first-succ (list func)
  (or (funcall func (car list))
      (first-succ (cdr list) func)))

(defun help-echo-at-point ()
  (interactive)
  (message
   (first-succ
    (overlays-at (point))
    (lambda (overlay)
      (overlay-get overlay 'help-echo)))))

(global-set-key (kbd "C-h C-m") 'help-echo-at-point)
(global-set-key (kbd "C->") 'flymake-goto-next-error)
(global-set-key (kbd "C-<") 'flymake-goto-prev-error)

(defun access-run ()
  (interactive)
  (let ((base (locate-dominating-file buffer-file-name "hanzo-warc-browser")))
    (if (not base)
	(error "Attempt to run access outside of access directory"))
    (cd (concat base "hanzo-warc-browser"))
    (compile "python browse.fcgi 8080")))

(defun python-doctest ()
  (interactive)
  (python-shell-send-buffer)
  (python-shell-send-string "import doctest")
  (python-shell-send-string "doctest.testmod()"))

(defun python-project-looks-like-setup (filename)
  (string-match "setup.py$" filename)
  (let ((cwd (directory-file-name (pwd))))
    (unwind-protect
	(progn
	  (cd (file-name-directory filename))
	  (shell-command-to-string (concat "python " filename " --name")))
      (cd cwd))))


(defun python-project-is-base (dirname))
  

(defun python-project-locate-setup (dirname))
  

(defun python-project-init (filename)
  (interactive)
  (make-local-variable 'python-project-base)
  (make-local-variable 'python-project-name)
  (make-local-variable 'python-project-setup-py)
  (python-project-locate-setup (file-name-directory filename)))

(defun python-run-tests ()
  (interactive)
  (let ((base (locate-dominating-file buffer-file-name "setup.py")))
    (if (not base)
	(error "Cannot find setup.py")
      (cd base)
      (compile "python setup.py test"))))


(defun maybe-access-run-hook ()
  (cond ((locate-dominating-file buffer-file-name "hanzo-warc-browser")
	 (local-set-key (kbd "C-c t") 'access-run))
	((locate-dominating-file buffer-file-name 'looks-like-setup)
	 (local-set-key (kbd "C-c t") 'python-run-tests))))

(defun add-to-pythonpath (path)
  (if (not (getenv "PYTHONPATH"))
      (setenv "PYTHONPATH" path)
    (let ((parts (split-string (getenv "PYTHONPATH") ":")))
      (if (not (member path parts))
	  (setenv
	   "PYTHONPATH"
	   (mapconcat 'identity (cons path parts) ":"))))))

(defun any-prefix-p (string &rest prefixes)
  (if prefixes
      (if (string-prefix-p (car prefixes) string)
	  't
	(apply 'any-prefix-p string (cdr prefixes)))))

(defun python-miyamoto-project-hook ()
  (if (and (buffer-file-name)
	   (any-prefix-p (buffer-file-name)
		     "/home/stephenjones/code/kagami"
		     "/home/stephenjones/code/miyamoto"))
      (add-to-pythonpath "/home/stephenjones/code/miyamoto")))

(add-hook 'python-mode-hook 'python-miyamoto-project-hook)

(defun my-python-mode-hook ()
  (setq insert-tabs-mode '())
  (flymake-mode)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (local-set-key (kbd "C-c f") 'python-describe-symbol)
  (local-set-key (kbd "C-c C-d") 'python-doctest)
  (setq ack-type "python")
  (make-local-variable 'w3m-search-default-engine)
  (setq w3m-search-default-engine "python")
  (add-hook 'local-write-file-hooks
	    '(lambda()
	       (save-excursion
		 (delete-trailing-whitespace)))))

(add-hook 'python-mode-hook 'my-python-mode-hook)
;;(add-hook 'python-mode-hook 'maybe-access-run-hook)

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

(define-skeleton cpp-boost-test-case
  "Insert a boost auto test case"
  "Name: "
  > "BOOST_AUTO_TEST_CASE(" str ") {" \n
  > _ \n
  "}" \n)

(defvar cpp-make-target "test")
(make-variable-buffer-local 'cpp-make-target)

(defun cpp-run-tests ()
  (interactive)
  (let ((make-path (expand-file-name (locate-dominating-file (buffer-file-name) "Makefile"))))
    (message make-path)
    (if (not make-path)
	(error "Couldn't locate makefile"))
    (compile (concat "cd " make-path " && make " cpp-make-target))))


(defun my-c++-mode-hook ()
  (flymake-mode)
  (hs-minor-mode)
  (local-set-key (kbd "C-c f") 'manual-entry)
  (local-set-key (kbd "C-c C-t") 'cpp-boost-test-case)
  (local-set-key (kbd "C-c C-m") 'compile)
  (local-set-key (kbd "C-c C-c") 'cpp-run-tests)
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
      (search-forward "1.")))

(defun google-w3m-find-results (url)
  (if (string-prefix-p "http://www.google.co.uk/search" url)
      (search-forward "1.")))

(add-hook 'w3m-display-hook 'postgres-w3m-find-results)
(add-hook 'w3m-display-hook 'google-w3m-find-results)

(defun postgres-get-help (symbol)
  (w3m (concat
	"http://www.postgresql.org/search/?u=%2Fdocs%2F9.1%2F&q="
	(url-encode symbol))))

(defvar postgres-help-history nil)

(defun postgres-help ()
  (interactive)
  (let ((query (get-symbol-prompt "Postgres docs" 'postgres-help-history)))
    (postgres-get-help query)))

(defvar psql-database-name)
(make-local-variable 'psql-database-name)

(defun psql-confirm-database-name ()
  (if (not (boundp 'psql-database-name))
      (setq psql-database-name (read-from-minibuffer "Database Name: " (getenv "USER")))))

(defun psql-set-database-name (name)
  (interactive "MDatabase Name: ")
  (setq psql-database-name name))

(defun psql-run-file ()
  (interactive)
  (psql-confirm-database-name)
  (compile (concat "psql '" psql-database-name "' -P pager -f " buffer-file-name)))


(defun psql-run-line ()
  (interactive)
  (let* ((beg (save-excursion
	       (beginning-of-line)
	       (point)))
	(end (save-excursion
	       (end-of-line)
	       (point)))
	(line (buffer-substring-no-properties beg end)))
    (shell-command (concat
		    "psql miyamoto -c '"
		    (replace-regexp-in-string "'" "'\\''" line)
		    "'"))))

(defun sql-string-escape (input)
  (replace-regexp-in-string "'" "''" input))

(defun psql-get-indexes ()
  (interactive)
  (let* ((table-name (symbol-name (symbol-at-point)))
	 (sql-command (concat "select indexname, indexdef from pg_indexes where tablename ='"
			      (sql-string-escape (downcase table-name))
			      "'")))
    (shell-command (concat
		    "psql miyamoto -c "
		    (shell-quote-argument sql-command))
		   (concat "*" table-name " indexes*"))))

(defvar postgres-block-starters '("select"))
(defvar postgres-block-keywords '("from"))
(defvar postgres-block-interesting (append postgres-block-starters postgres-block-keywords))

(defun postgres-begining-of-block ()
  (interactive)
  (let ((start-re (regexp-opt (cons ")" postgres-block-starters))))
    (search-backward-regexp start-re)
    (while (string-equal (match-string 0) ")")
      (forward-char)
      (backward-sexp)
      (search-backward-regexp start-re))))


(defun postgres-indent ()
  (interactive)
  (let ((interesting-re (regexp-opt postgres-block-interesting))
	(block-indent 0))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
	  (setq block-indent 0)
	(beginning-of-line)
	(if (looking-at interesting-re)
	    (progn
	      (previous-line)
	      (setq block-indent 
	  
	(while (not (looking-at interesting-re))
	  (previous-line))
	(setq block-indent (current-indentation))))
    (indent))))))

(defun current-error ()
  (interactive)
  (with-current-buffer next-error-last-buffer
    (let* ((msg (compilation-next-error 0))
	   (loc (compilation--message->loc msg))
	   (end-loc (compilation--message->end-loc msg)))
      (buffer-substring loc end-loc))))

(require 'sql)

(defun my-sql-mode-hook ()
  (local-set-key (kbd "C-c f") 'postgres-help)
  (local-set-key (kbd "C-c C-p") 'sql-connect)
  (local-set-key (kbd "C-c C-c") 'sql-send-buffer)
  (local-set-key (kbd "C-c C-r") 'sql-send-region)
  (local-set-key (kbd "C-c C-s") 'sql-send-string)
  (sql-set-product 'postgres)
  (setq ack-type "sql")
  (make-local-variable 'w3m-search-default-engine)
  (setq w3m-search-default-engine "postgres")
  (flyspell-prog-mode))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)

(global-set-key (kbd "C-x 8 A") "∀")
(global-set-key (kbd "C-x 8 E") "∃")
(global-set-key (kbd "C-x 8 a") "∧")
(global-set-key (kbd "C-x 8 o") "∨")
(global-set-key (kbd "C-x 8 U") "∪")
(global-set-key (kbd "C-x 8 I") "∩")
(global-set-key (kbd "C-x 8 d") "⋅")
(global-set-key (kbd "C-x 8 c") "∘")
(global-set-key (kbd "C-x 8 x") "×")
(global-set-key (kbd "C-x 8 r") "→")
(global-set-key (kbd "C-x 8 l") "←")
(global-set-key (kbd "C-x 8 R") "⇢")
(global-set-key (kbd "C-x 8 L") "⇠")
(global-set-key (kbd "C-x 8 u") "μ")

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

(add-to-list 'compilation-error-regexp-alist-alist
	     '(cheetah-template-error "^Line \\([[:digit:]]+\\), column \\([[:digit:]]+\\) in file \\(.*\\)$"
				      3 1 2))
(add-to-list 'compilation-error-regexp-alist 'cheetah-template-error)

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

(defun filter (p l)
  (if (not l)
      ()
    (if (funcall p (car l))
	(cons (car l) (filter p (cdr l)))
      (filter p (cdr l)))))

(defmacro remove-from-list (l p)
  (set 'l (filter p l)))

;(setq foo '(1 2 3 4))
;(remove-from-list foo (lambda (x) (> x 2)))
;foo

(defun careq (a b)
  (equalp (car a) (car b)))

(add-to-list 'compilation-error-regexp-alist-alist
	     '(psql-notice "^psql:\\([^:]+\\):\\([[:digit:]]+\\):\\s-+\\(NOTICE\\):\\s-+\\(.*\\)" 1 2 nil 0 4)
	     nil
	     'careq)
(add-to-list 'compilation-error-regexp-alist-alist
	     '(psql-error "^psql:\\([^:]+\\):\\([[:digit:]]+\\):\\s-+\\(ERROR\\):\\s-+\\(.*\\)" 1 2 nil 2 4 (3 'error)))
(add-to-list 'compilation-error-regexp-alist-alist
	     '(psql-function "^PL/pgSQL function \"\\([^\"]+\\)\" line \\([[:digit:]]+\\)" nil 2 nil nil nil (1 'font-lock-function-name-face)))
(add-to-list 'compilation-error-regexp-alist 'psql-notice)
(add-to-list 'compilation-error-regexp-alist 'psql-error)
(add-to-list 'compilation-error-regexp-alist 'psql-function)

(setq gnus-select-method 
      '(nnimap "mail.secretvolcanobase.org"
	       (nnimap-address "mail.secretvolcanobase.org")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(require 'smtpmail)
;(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-message-face (quote highlight))
 '(compilation-skip-threshold 1)
 '(compilation-window-height 20)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".orig")))
 '(confluence-default-space-alist (quote (("https://hq.hanzoarchives.com/confluence/rpc/xmlrpc" . "dashboard"))))
 '(confluence-url "https://hq.hanzoarchives.com/confluence/rpc/xmlrpc")
 '(dvc-confirm-add nil)
 '(dvc-tips-enabled nil)
 '(fill-column 78)
 '(flymake-fringe-indicator-position (quote right-fringe))
 '(flymake-log-level 0)
 '(flymake-warning-bitmap (quote (question-mark warning)))
 '(global-ede-mode nil)
 '(global-hl-line-mode t)
 '(gnus-ignored-newsgroups "")
 '(gnus-select-method (quote (nnimap "mail.secretvolcanobase.org")))
 '(gnus-summary-line-format "%U%R%z%>%(%[%-23,23f%]%) %s
")
 '(hs-hide-comments-when-hiding-all nil)
 '(ido-auto-merge-delay-time 99999)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-auto-rescan t)
 '(ispell-dictionary "british")
 '(ispell-local-dictionary "british")
 '(jira-url "https://hq.hanzoarchives.com/jira/rpc/xmlrpc")
 '(max-mini-window-height 2)
 '(mm-text-html-renderer (quote w3m))
 '(mm-verify-option (quote always))
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
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/notes.el")
 '(org-habit-show-habits-only-for-today nil)
 '(org-habit-today-glyph 84)
 '(org-mobile-directory "/svb:org-mobile")
 '(org-mobile-inbox-for-pull "~/org/from-mobile.org")
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 10))))
 '(org-src-fontify-natively t)
 '(org-tag-persistent-alist (quote ((:startgroup) ("@code" . 99) ("@home" . 104) ("@town" . 116) (:endgroup) ("bill" . 98) ("shopping" . 115))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(read-mail-command (quote gnus))
 '(remember-data-file "~/remember.org")
 '(safe-local-variable-values (quote ((cpp-make-target . "run-problem7") (cpp-make-target . "run-problem6") (cpp-make-target . "run-problem5") (cpp-make-target . "run-problem4") (cpp-make-target . "run-problem3") (cpp-make-target . "run-problem2") (cpp-make-target . "run-problem1") (cpp-make-target . "problem1") (cpp-make-target . problem1))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(smtpmail-debug-info t)
 '(smtpmail-local-domain nil)
 '(smtpmail-smtp-server "mail.secretvolcanobase.org")
 '(smtpmail-smtp-service "smtp")
 '(smtpmail-smtp-user "zombywuf")
 '(smtpmail-stream-type (quote starttls))
 '(sql-connection-alist (quote (("miyamoto" (sql-product (quote postgres)) (sql-user "stephenjones") (sql-server "") (sql-database "miyamoto")) ("stephenjones" (sql-product (quote postgres)) (sql-user "stephenjones") (sql-database "stephenjones") (sql-server "")) ("kagami" (sql-product (quote postgres)) (sql-user "stephenjones") (sql-database "kagami") (sql-server "")))))
 '(user-mail-address "steve@secretvolcanobase.org")
 '(vc-delete-logbuf-window nil)
 '(w3m-key-binding (quote info))
 '(w3m-search-engine-alist (quote (("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil) ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan) ("alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8) ("blog" "http://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8) ("blog-en" "http://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8) ("google" "http://www.google.co.uk/search?q=%s&ie=utf-8&oe=utf-8" utf-8) ("google-en" "http://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8) ("google news" "http://news.google.co.jp/news?hl=ja&ie=utf-8&q=%s&oe=utf-8" utf-8) ("google news-en" "http://news.google.com/news?hl=en&q=%s" nil) ("google groups" "http://groups.google.com/groups?q=%s" nil) ("All the Web" "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s" nil) ("All the Web-ja" "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp&q=%s" euc-japan) ("technorati" "http://www.technorati.com/search/%s" utf-8) ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8) ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8) ("goo-ja" "http://search.goo.ne.jp/web.jsp?MT=%s" euc-japan) ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis) ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil) ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil) ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil) ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil) ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan) ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice" nil) ("waei" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan) ("eiwa" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej" nil) ("kokugo" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan) ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67" nil) ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s") ("amazon-ja" "http://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis) ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil) ("en.wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil) ("de.wikipedia" "http://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8) ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8) ("msdn" "http://search.msdn.microsoft.com/search/default.aspx?query=%s" nil) ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil) ("postgres" "http://www.postgresql.org/search/?u=%%2Fdocs%%2F9.1%%2F&q=%s" utf-8) ("python" "http://docs.python.org/search.html?q=%s&check_keywords=yes&area=default" utf-8))))
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Bitstream Vera Sans Mono"))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(comint-highlight-prompt ((t nil)))
 '(error ((t (:foreground "orangered" :weight bold))))
 '(flymake-errline ((((class color) (min-colors 88) (background dark)) (:underline "red")) (((class color) (background light)) (:underline "red"))))
 '(flymake-warnline ((t (:underline "steelblue1" :slant italic))))
 '(flyspell-duplicate ((t (:underline "gold4"))))
 '(flyspell-incorrect ((t (:underline "darkred"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "lightsteelblue")) (((class color) (min-colors 88) (background light)) (:foreground "blue1"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#6b969a"))))
 '(font-lock-comment-face ((t (:foreground "#a0e0e6" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#8db27b"))))
 '(font-lock-doc-face ((t (:foreground "#a0e0e6"))))
 '(font-lock-function-name-face ((t (:foreground "#adb1ec"))))
 '(font-lock-keyword-face ((t (:foreground "#ffc896"))))
 '(font-lock-string-face ((t (:foreground "#7ac6cd"))))
 '(font-lock-type-face ((t (:foreground "#acee8c"))))
 '(font-lock-variable-name-face ((t (:foreground "#7779a2"))))
 '(gnus-header-content ((t (:foreground "indianred4"))))
 '(highlight ((t (:background "grey20"))))
 '(italic ((t (:slant italic))))
 '(mode-line ((t (:background "grey25" :foreground "grey90"))))
 '(mode-line-buffer-id ((t (:background "grey40" :weight bold))))
 '(mode-line-inactive ((t (:background "grey10" :foreground "grey80" :weight light))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :height 1.5))) t)
 '(org-agenda-date-today ((t (:inherit org-agenda-date :background "gray30" :slant italic :weight bold))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "grey20"))) t)
 '(org-agenda-done ((t (:foreground "dark green"))))
 '(org-agenda-structure ((t (:inherit org-level-1 :box (:line-width 8 :color "black")))))
 '(org-block-background ((t (:background "#2e328c"))))
 '(org-column ((t (:background "grey20" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono"))))
 '(org-date ((t (:foreground "#8c90d8"))))
 '(org-done ((t (:background "ForestGreen" :foreground "white" :box (:line-width 2 :color "darkgreen" :style released-button) :weight bold))))
 '(org-habit-alert-face ((t (:background "#8c90d8"))))
 '(org-habit-alert-future-face ((t (:background "#7779A2"))))
 '(org-habit-clear-face ((t (:background "black"))))
 '(org-habit-clear-future-face ((t (:background "black"))))
 '(org-habit-overdue-face ((t (:background "#ffc896"))))
 '(org-habit-overdue-future-face ((t (:background "#bfA084"))))
 '(org-habit-ready-face ((t (:background "#7AC6CD"))))
 '(org-habit-ready-future-face ((t (:background "#6B969A"))))
 '(org-scheduled-previously ((t (:foreground "#ffc896"))))
 '(org-scheduled-today ((t (:foreground "#acee8c"))))
 '(org-table ((t (:foreground "#acee8c"))))
 '(org-tag ((t (:background "grey29" :slant italic))))
 '(org-todo ((t (:background "red1" :foreground "white" :box (:line-width 2 :color "red4" :style released-button) :weight bold))))
 '(org-upcoming-deadline ((t (:inherit default :weight bold))))
 '(outline-1 ((t (:foreground "#B3E2E6" :weight bold :height 1.3))))
 '(outline-2 ((t (:foreground "#A0E0E6" :weight bold :height 1.1))))
 '(outline-3 ((t (:foreground "#7Ac6cd" :weight bold))))
 '(outline-4 ((t (:foreground "#6b969a" :weight bold))))
 '(outline-5 ((t (:foreground "#287d85" :weight bold))))
 '(outline-6 ((t (:foreground "#287d85" :weight bold))))
 '(outline-7 ((t (:foreground "#287d85" :weight bold))))
 '(which-func ((t (:inherit font-lock-function-name-face)))))

(put 'narrow-to-region 'disabled nil)
