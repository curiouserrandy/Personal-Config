;;; Want the following behavior:
;;;	* Base command used without history; command portion
;;;	  of last in history used with history.  ?? How do we separate
;;;	  tag and command portions of history ??
;;;	* Default included tag under point if prefix arg, not if not.
;;;	* Can reverse default with an argument.

(defvar glimpse-history nil "History variable for glimpse runs.")
(defvar egrep-history nil "History variable for egrep runs.")
(defvar fsfile-history nil "History variable for fsfile runs.")
(defvar rgrep-history nil "History variable for rgrep runs.")

;; Translation: tag surrounded by ' OR tag surrounded by " OR non-whitespace 
;; contiguous tag at end of line.
(defconst grep-like-command-tag-regexp
  "\\(\'[^\']*\'\\|\"[^\"]*\"\\|[^\\s ]*\\)\\s *$"
  "Regular expression to use for finding tag match in previous calls to
this function.")

(defun grep-like-command (command invert-tag-default history-symbol)
  "Run a command in a grep fashion.  
COMMAND is the base command, to be used if history is null.  
INVERT-TAG-DEFAULT indicates whether or not we want the
default to be prompting with tag or not.
HISTORY is bound to the symbol to use as the history to use for this command.
To modify the devnull behavior call this function inside a context in 
which grep-null-device is nil.
To use a different regexp for parsing out the tag, do the same with
grep-like-command-tag-regexp."
  (let* ((get-tag current-prefix-arg)
	 (history (eval history-symbol))
	 (last-command (and history (car history)))
	 (tag-default
	  (progn
	    (if invert-tag-default (setq get-tag (not get-tag)))
	    (and get-tag
		 (funcall (or find-tag-default-function
			      (get major-mode 'find-tag-default-function)
			      'find-tag-default)))))
	 (default-command (concat command " " tag-default))
	 editted-command
	 )
    (if last-command
	(progn
	  (if (string-match grep-like-command-tag-regexp last-command)
	      (setq default-command (replace-match tag-default t t
						   last-command 1)))))
    (setq editted-command
	  (read-from-minibuffer "Run command (like this): "
				default-command
				nil nil history))
    (grep editted-command)))

(defconst rgrep-command-format-string "rgrep"
  "String to use as base for recursive egrep runs.")

(defun randy-run-egrep-recursively ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command rgrep-command-format-string t 'egrep-history)))

(defconst fsfile-command-string "fsfile -a -s "
  "String to use as a base for finding file names.")

(defun randy-run-fsfile ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command (concat fsfile-command-string randy-sandbox-directory)
		       t 'fsfile-history)))

(defvar glimpse-base-command "glimpse -n -y -H"
  "Default command to use for running glimpse.")

(defvar glimpse-baes-directory nil "Directory at which to target glimpse")

(defun randy-run-glimpse ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command
     (concat glimpse-base-command glimpse-base-directory)
     t 'glimpse-history)
    ;; Set the default directory of the grep buffer to the glimpse
    ;; base directory so relative paths work.
    (set-buffer "*grep*")
    (cd glimpse-base-directory)))

(provide 'rs-grepplusplus)

