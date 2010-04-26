;;; Want the following behavior:
;;;	* Base command used without history; command portion
;;;	  of last in history used with history.  ?? How do we separate
;;;	  tag and command portions of history ??
;;;	* Default included tag under point if prefix arg, not if not.
;;;	* Can reverse default with an argument.


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

			    

