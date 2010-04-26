;; To use glimpse
(require 'compile)
(defvar grep-glimpse-history nil)
(defvar grep-glimpse-command
  (concat "/u/randy/bin/i386-BSD_OS/glimpse -n "
	  (if glimpse-default-directory
	      (concat "-H" glimpse-default-directory " ")
	    ""))
  "The default glimpse command for \\[grep-glimpse].")

;; Much of this is shamelessly copied from the grep function.
;;;(defun grep-glimpse (command-args)
;;;  "Run glimpse, with user-specified args, and collect output in a buffer.
;;;While grep runs asynchronously, you can use the \\[next-error] command
;;;to find the text that grep hits refer to.
;;;
;;;This command uses a special history list for its arguments, so you can
;;;easily repeat a grep command."
;;;  (interactive
;;;   (list (read-from-minibuffer "Run glimpse (like this): "
;;;			       grep-glimpse-command nil nil 'grep-glimpse-history)))
;;;  (let ((buf (compile-internal command-args
;;;			       "No more glimpse hits" "glimpse"
;;;			       ;; Give it a simpler regexp to match.
;;;			       nil grep-regexp-alist)))
;;;    (save-excursion
;;;      (set-buffer buf)
;;;      (set (make-local-variable 'compilation-exit-message-function)
;;;	   (lambda (status code msg)
;;;	     (if (eq status 'exit)
;;;		 (cond ((zerop code)
;;;			'("finished (matches found)\n" . "matched"))
;;;		       ((= code 1)
;;;			'("finished with no matches found\n" . "no match"))
;;;		       (t
;;;			(cons msg code)))
;;;	       (cons msg code)))))))


(defun grep-glimpse (command-args)
  "Run glimpse so that it looks like grep, with user-specified args,
and collect output in a buffer.
While glimpse runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."
  (interactive
   (let (glimpse-default (arg current-prefix-arg))
     (if arg
       (let* ((tag-default
	       (funcall (or find-tag-default-function
			    (get major-mode 'find-tag-default-function)
			    'find-tag-default))))
	 ;; If there's a history we replace the default tag.  If there isn't,
	 ;; we simply concatenate.
	 (if (car grep-glimpse-history)
	     (progn
	       (setq glimpse-default (car grep-glimpse-history))
	       (if (string-match "[^ ]+\\s +\\(-[^ ]+\\s +\\)*\\(\"[^\"]+\"\\|[^ ]+\\)"
				 glimpse-default)
		   (setq glimpse-default (replace-match tag-default t t
							glimpse-default 2))))
	   (setq glimpse-default (concat grep-glimpse-command tag-default)))))

     (list (read-from-minibuffer "Run glimpse (like this): "
				 (or glimpse-default grep-glimpse-command)
				 nil nil
				 'grep-glimpse-history))))
  (let ((grep-null-device nil))		; see grep
    (grep command-args)))

(define-key mode-specific-map "i" 'grep-glimpse)
