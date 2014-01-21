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

(defvar web-search-history nil)

(defun search-web (query)
  (interactive "s Search Web: ")
  (browse-url (concat "https://duckduckgo.com/?q=" (url-encode query))))

(defun search-web-at-point ()
  "Do a google search with a default query of the symbol at the point."
  (interactive)
  (search-web (get-symbol-prompt "Search Web" 'web-search-history)))

(global-set-key (kbd "C-c g") 'search-web-at-point)

(global-set-key "\C-xm" 'browse-url-at-point)
