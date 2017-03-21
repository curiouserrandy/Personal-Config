(require 'cc-mode)

(defun randy-word-under-point (&optional syntab wordsep)
  "Return a string which is the word underneath point in the current buffer.
Optionally specify an alternate syntax table in SYNTAB to define words.
Optionally specify a word separator WORDSEP.  If specified, the function
will return a list of words connected by the separator character.
may return either a single word or a list of two words (the second
following the word point is on separated from it by WORDSEP).

If the mark is active, returns the selected text."
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (let* ((buffer-syntax-table (syntax-table))
	   (local-syntax-table (or syntab buffer-syntax-table))
	   word1 wordaft)
      (save-excursion
	(set-syntax-table local-syntax-table)
	(setq word1 (buffer-substring-no-properties
		     (progn (forward-word -1) (point))
		     (progn (forward-word 1) (point))))
	(if (equal (char-after (point)) wordsep)
	    (progn
	      (forward-char 1)
	      (setq wordaft (buffer-substring (point)
					      (progn (forward-word 1)
						     (point))))))
	(set-syntax-table buffer-syntax-table)
	(if wordaft
	    (list word1 wordaft)
	  word1)))))

(defconst randy-find-file-under-point-syntab
  (let ((syntab (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" syntab)
    (modify-syntax-entry ?- "w" syntab)
    (modify-syntax-entry ?+ "w" syntab)
    (modify-syntax-entry ?_ "w" syntab)
    (modify-syntax-entry ?/ "w" syntab)
    (modify-syntax-entry ?~ "w" syntab)
    syntab)
  "Syntax table to use for distinguishing filenames in text.")

(defun randy-find-file-under-point (other-window-p)
  "Find file under point.  With a prefix arg, find it in the other window."
  (interactive "P")
  (let ((filename (randy-word-under-point randy-find-file-under-point-syntab ?:))
	wordafter)
    (if (consp filename)
	(progn
	  (setq wordafter (car (cdr filename)))
	  (setq filename (car filename))))
    (if (string-match "^.*\\.$" filename)
	(setq filename (substring filename 0 -1)))
    (if other-window-p
	(find-file-other-window filename)
      (find-file filename))
    (if (and wordafter (string-match "^[0-9]*$" wordafter))
	(goto-line (string-to-number wordafter)))
    (if other-window-p
	(other-window 1))
    ))

(defun randy-alternative-find-file-under-point (other-window-p)
  "Find file (or file:line, or general URL) under point.
With prefix arg, find it in the other window."
  (interactive "P")
  (save-excursion
    (if (re-search-backward "\n\\| \\|	\\|\\[\\|\"" (point-min) 1)
	(goto-char (match-end 0))
      ;; Else will just go to beginning of file and stop, which may be right
      )
    (if (looking-at "(")
	(forward-char 1))
    (cond
     ;; URL of some sort
     ((looking-at "\\(https?\\|ftp\\|file\\):[^\] 	
]*")
      (let ((urlname (buffer-substring (match-beginning 0) (match-end 0))))
	
	(if (string-match "\\.$" urlname)
	    (setq urlname (substring urlname 0 -1)))
	(if (string-match ")$" urlname)
	    (setq urlname (substring urlname 0 -1)))
	(message "Opening URL: %s" urlname)
	(browse-url urlname)))
     ;; Generic file on local file system
     ((looking-at "\\([-a-zA-Z0-9._+/~]+\\)\\(:\\([0-9]+\\)\\)?")
      (let ((filename (buffer-substring (match-beginning 1) (match-end 1)))
	    (line (and (not (equal (match-beginning 3) (match-end 3)))
		       (string-to-number (buffer-substring (match-beginning 3)
							   (match-end 3))))))
	;; Special case editing
	(if (and (string-match "\\.$" filename) (not line))
	    (setq filename (substring filename 0 -1)))
	(if other-window-p
	    (find-file-other-window filename)
	  (find-file filename))
	(if line (goto-line line))))
     (t (error "Couldn't recognize text under point as a file or URL.")))))

;;; XXX: If find-tag-default has been changed, possibly should use changed name.
(defun randy-query-change-name (original_name new_name)
  "Changes a name everywhere in the buffer.  
Default name to change from is the name under point."
  (interactive
   (let*
      ((default-start (find-tag-default))
       (oname (read-from-minibuffer "Change name: " default-start))
       (nname (read-from-minibuffer (concat "Change name: " oname " to: ")
				    default-start)))
    (list oname nname)))
  (save-excursion
    (goto-char (point-min))
    (query-replace original_name new_name)))

(provide 'rs-underpoint)
