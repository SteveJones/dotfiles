(defun python-safe-string (string)
  (setq string (replace-regexp-in-string "\\\\" "\\\\\\\\" string))
  (setq string (replace-regexp-in-string "\'" "\\\\'" string))
  (setq string (replace-regexp-in-string "\"" "\\\\\"" string))
  string)

(defun python-show-buffer ()
  (interactive)
  (display-buffer python-buffer))

(defun python-goto-buffer ()
  (interactive)
  (switch-to-buffer-other-window python-buffer))

(defun python-doctest-buffer ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (with-temp-buffer
      (insert "def __runtests():
    import doctest
    r = doctest.testfile(filename=\"")
      (insert (python-safe-string file-name))
      (insert "\", module_relative=False)

__runtests()")
      (python-send-buffer)))
  (python-show-buffer))

(defun python-send-paragraph (&optional display)
  (interactive "P")
  (save-excursion
    (mark-paragraph)
    (python-send-region (point) (mark)))
  (if display
      (python-show-buffer)))

(defun python-keybinding-hook ()
  (local-set-key (kbd "C-c t b") 'python-doctest-buffer)
  (local-set-key (kbd "M-g s") 'python-show-buffer)
  (local-set-key (kbd "M-g i") 'python-goto-buffer)
  (local-set-key (kbd "C-c C-e") 'python-send-defun))

(add-hook 'python-mode-hook 'python-keybinding-hook)

(defun python-minor-mode-hook ()
  (flymake-mode)
  (hs-minor-mode)
  (flyspell-prog-mode))

(add-hook 'python-mode-hook 'python-minor-mode-hook)

(defun python-config-hook ()
  (setq ack-type "python")
  (add-hook 'local-write-file-hooks
	    '(lambda ()
	       (save-excursion
		 (delete-trailing-whitespace)))))

(add-hook 'python-mode-hook 'python-config-hook)
