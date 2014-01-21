(require 'sql)

(defun postgres-search-web (query)
  (browse-url 
   (concat "http://www.postgresql.org/search/?u=%2Fdocs%2F9.1%2F&q="
	   (url-encode query))))

(defvar postgres-help-history nil)

(defun postgres-help ()
  (interactive)
  (let ((query (get-symbol-prompt "Postgres docs" 'postgres-help-history)))
    (postgres-search-web query)))

(defvar psql-database-name)
(make-local-variable 'psql-database-name)

(defun sql-buffer-get-product (buf)
  (and (sql-buffer-live-p buf)
       (with-current-buffer buf
	 sql-product)))

(defun sql-buffer-get-connection (buf)
  (and (sql-buffer-live-p buf)
       (with-current-buffer buf
	 sql-connection)))

(defun sql-get-buffers (&optional product connection)
  (filter (lambda (buffer)
	    (sql-buffer-live-p buffer product connection))
	  (buffer-list)))

(defvar sql-read-connection-history nil)

(defun sql-read-connection (prompt &optional initial default)
  "Read a connection name."
  (let ((connection-name (ido-completing-read
			  "Connection name: "
			  (mapcar (lambda (con) (car con)) sql-connection-alist)
			  nil 't initial
			  'sql-read-connection-history
			  default)))
    (add-to-list 'sql-read-connection-history connection-name)
    connection-name))

(defun sql-set-sqli-buffer ()
  (interactive)
  (let* ((buffers (sql-get-buffers))
	 (buffer (ido-completing-read
		  "Select buffer: "
		  (mapcar (lambda (buffer)
			    (buffer-name buffer)) buffers) nil 't)))
    (setq sql-buffer buffer)
    (run-hooks 'sql-set-sqli-hook)))

(defconst sql-beginning-of-defun-regexp "create[[:space:]\n]+\\(or[[:space:]\n]+replace[[:space:]\n]+\\)?function")

(defun sql-beginning-of-defun (&optional arg)
  (interactive "P")
  (search-forward ";" nil 't)
  (search-backward-regexp sql-beginning-of-defun-regexp nil nil arg))

(defun sql-forward-name (&optional arg)
  (interactive "P")
  (message "sql-forward-name")
  (cond ((not arg)
	 (sql-forward-name 1))
	((= 0 arg)
	 't)
	((< arg 0)
	 (sql-backward-name (- 0 arg)))
	((> arg 0)
	 (forward-symbol 1)
	 (if (looking-at "[[:space:]\n]*\\.")
	     (forward-symbol 1))
	 (sql-forward-name (- arg 1)))))

(defun sql-forward-token ()
  (interactive)
  (cond ((looking-at "[[:space:]\n]*'")
	 (forward-sexp))
	((looking-at "[[:space:]\n]*\\$\\([^\\$]*\\)\\$")
	 (search-forward (concat "$" (match-string 1) "$") nil nil 2))
	('t (forward-symbol 1))))

(defun sql-end-of-defun (&optional arg)
  (interactive "P")
  (if (not (looking-at sql-beginning-of-defun-regexp))
      (sql-beginning-of-defun))

  (if (and (not (looking-at sql-beginning-of-defun-regexp)) ; If we started before any defuns
	   (not (search-forward-regexp sql-beginning-of-defun-regexp nil nil)))
      nil ; Can't find a defun start
    (if (not (search-forward "("))
	nil)
    (backward-char)
    (forward-sexp)  	   ; After the argument list
    (while (not (looking-at "[[:space:]\n]*;"))
      (sql-forward-token))
    (search-forward ";")))

(defun sql-send-defun ()
  (interactive)
  (let ((begin (save-excursion
		 (beginning-of-defun)
		 (point)))
	(end (save-excursion
	       (end-of-defun)
	       (point))))
    (sql-send-region begin end)))

(defun sql-define-token ()
  (interactive)
  (let ((command (concat "\n\\d " (symbol-name (symbol-at-point)) ";")))
    (message command)
    (sql-send-string command)))

(defun sql-defun-goto-line (line)
  (interactive "P")
  (if (not line)
      (setq line (read-number "Line: ")))
  (sql-beginning-of-defun)
  (backward-char 1)
  (forward-line line))

(defun sql-switch-to-interactive ()
  (interactive)
  (if (sql-buffer-live-p sql-buffer)
      (pop-to-buffer sql-buffer)))

(defun string-font-lock ()
  (interactive)
  (add-to-list 'font-lock-keywords (cons "\\$[^\\$]*\\$" font-lock-string-face)))

(defun my-sql-mode-hook ()
  (string-font-lock)
  (local-set-key (kbd "C-c f") 'postgres-help)
  (local-set-key (kbd "C-c C-p") 'sql-connect)
  (local-set-key (kbd "C-c C-c") 'sql-send-paragraph)
  (local-set-key (kbd "C-c C-l") 'sql-send-buffer)
  (local-set-key (kbd "C-c C-r") 'sql-send-region)
  (local-set-key (kbd "C-c C-s") 'sql-send-string)
  (local-set-key (kbd "C-c C-d") 'sql-send-defun)
  (local-set-key (kbd "C-c d") 'sql-define-token)
  (local-set-key (kbd "C-c C-b") 'sql-set-sqli-buffer)
  (local-set-key (kbd "M-g d") 'sql-defun-goto-line)
  (local-set-key (kbd "M-g i") 'sql-switch-to-interactive)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (setq beginning-of-defun-function 'sql-beginning-of-defun
	end-of-defun-function 'sql-end-of-defun)
  (sql-set-product 'postgres)
  (setq ack-type "sql")
  (make-local-variable 'w3m-search-default-engine)
  (setq w3m-search-default-engine "postgres")
  (flyspell-prog-mode))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)

(defun psql-hook ()
  (setq sql-prompt-regexp "^\\(\\S-+:\\)?\\S-+@\\S-+/\\S-+# "
	sql-prompt-cont-regexp "^\\S-+[(\\$-]# "))

(add-hook 'sql-interactive-mode-hook 'psql-hook)
