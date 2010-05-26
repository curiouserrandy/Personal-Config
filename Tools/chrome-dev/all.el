
;; Try and setup C++ bindings properly for chrome
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq large-file-warning-threshold
      (max large-file-warning-threshold (* 25 1024 1024))) ;For TAGS file.

(defconst chrome-issue-list-url-format-string
    "http://code.google.com/p/chromium/issues/list?can=2&q=%s&colspec=ID+Stars+Pri+Area+Feature+Type+Status+Summary+Modified+Owner+Mstone+OS&x=mstone&y=area&cells=tiles"
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

(defun chrome-visit-issue-list (issue-list)
  (browse-url (chrome-issue-list-url issue-list)))

(defun chrome-visit-issue (issue)
  (browse-url (format "http://crbug.com/%d" issue)))
