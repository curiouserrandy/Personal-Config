
;; Try and setup C++ bindings properly for chrome
(require 'google-c-style)
(require 'rs-underpoint)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Also make sure that when we save a C style file, we eliminate trailing
;; spaces.
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (add-hook (make-local-variable 'before-save-hook)
		       '(lambda ()
			  (save-excursion
			    (goto-char (point-min))
			    (replace-regexp "[ 	][ 	]*$" ""))))))

(setq large-file-warning-threshold
      (max large-file-warning-threshold (* 25 1024 1024))) ;For TAGS file.

(defconst chrome-issue-list-url-format-string
    "http://code.google.com/p/chromium/issues/list?can=1&q=%s&colspec=ID+Stars+Pri+Area+Feature+Type+Status+Summary+Modified+Owner+Mstone+OS&x=mstone&y=area&cells=tiles"
    "String to use for displaying sets of issues in the chrome issue tracker in list form.")

(defun chrome-issue-list-url (issue-list)
  "Return the URL to use for displaying the list of ISSUES passed in.
ISSUES is a list of integer values."
  (format chrome-issue-list-url-format-string
	  (mapconcat 'identity
		     (mapcar '(lambda (issue) (concat "id:"
						      (number-to-string issue)))
			     issue-list)
		     "+OR+")))

(defun chrome-visit-issue-list (&rest issue-list)
  (if (and (equal (length issue-list) 1)
	   (listp (car issue-list)))
      (setq issue-list (car issue-list))) ; List as the first arg
  (browse-url (chrome-issue-list-url issue-list)))

(defun chrome-visit-all-issues (&rest issue-list)
  (if (and (equal (length issue-list) 1)
	   (listp (car issue-list)))
      (setq issue-list (car issue-list))) ; List as the first arg
  (while issue-list
    (let ((this-issue (car issue-list)))
      (chrome-visit-issue this-issue)
      (setq issue-list (cdr issue-list)))))

(defun chrome-visit-issue-range (start end)
  (let ((bugid start) issue-list)
    (while (< bugid end)
      (setq issue-list (cons bugid issue-list))
      (setq bugid (+ bugid 1)))
    (chrome-visit-issue-list issue-list)))

(defun chrome-visit-issue (issue)
  (interactive
   (list (string-to-number (randy-word-under-point))))
  (if (< issue 1000000)
      ;; Bug
      (browse-url (format "http://crbug.com/%d" issue))
    ;; CL
    (browse-url (format "http://codereview.chromium.org/%d" issue))))


(defun chrome-visit-issues-mentioned-in-file ()
  "Scan through the file looking for lines of the form '^\*+ \d+:' and
bring each such chrome issue up in the browser."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+[ 	]+\\([0-9]+\\):" (point-max) t)
      (chrome-visit-issue
       (string-to-number
	(buffer-substring (match-beginning 1) (match-end 1)))))))

;; Setup git bindings
(randy-init-from "Tools/git")

(provide 'chrome-dev)
