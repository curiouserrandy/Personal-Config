
;; Try and setup C++ bindings properly for chrome
(require 'google-c-style)
(require 'rs-underpoint)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Make sure objective C shows up before nroff :-}
(setq auto-mode-alist (cons '("\\.mm" . objc-mode) auto-mode-alist))

(defun editing-chrome-file-p ()
  "Return t/nil depending on whether this is a file in a chromium
repository."
  (let* ((path (buffer-file-name)) (pathdir (file-name-directory path)))
    (and path
	(progn
	  (while (and pathdir
		      (not (equal "/" pathdir))
		      (not (file-exists-p (concat pathdir "/.git"))))
	    (setq pathdir (file-name-directory (directory-file-name pathdir))))
	  (and (file-exists-p (concat pathdir "/.git"))
	       (file-exists-p (concat pathdir "/chrome")))))))

;; Also make sure that when we save a C or python style file, we eliminate
;; trailing spaces.
(let ((this-hook '(lambda ()
		    (add-hook (make-local-variable 'before-save-hook)
			      '(lambda ()
				 (if (editing-chrome-file-p)
				     (save-excursion
				       (goto-char (point-min))
				       (replace-regexp "[ 	][ 	]*$" ""))))))))
  (add-hook 'c-mode-common-hook this-hook)
  (add-hook 'python-mode-hook this-hook))

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

(defconst chrome-test-regexp  "[A-Z_]*TEST[^(]*(\\([A-Za-z0-9_]*\\),[ 	
]*\\([A-Za-z0-9_]*\\))"
  "Regexp to match tests.")

(defun chrome-filepath-from-src ()
  "Return the path to the current file from the 'src' directory."
  (let* ((filepath (buffer-file-name))
	 (filename (file-name-nondirectory filepath)))
    (if (not (string-match "/src/\\(.*\\)$" filepath))
	nil
      (substring filepath (match-beginning 1)))))
	
(defun chrome-blame-file ()
  "Visit the web based blame facility for the current file."
  (interactive)
  (let ((path-from-src (chrome-filepath-from-src))
	(lineno (line-number-at-pos)))
    (if (not path-from-src)
	(error "Coudln't find /src directory in test file path %s."
	       (buffer-file-name)))

  (browse-url
   (format "https://chromium.googlesource.com/chromium/src/+blame/master/%s#%d"
	   path-from-src lineno))))

(defun chrome-speciality-compile ()
  "If in a test, compile and run that test.  If not in a test, compile
just the current file."
  (let ((in-test t)
	(path-from-src (chrome-filepath-from-src))
	(filename (file-name-nondirectory (buffer-file-name)))
	testname dirdepth path-to-src 
	path-elements test_executable_name)
  (save-excursion
    ;; Figure out the path to the src; needed for both forks.
    (if (not path-from-src)
	(error "Coudln't find /src directory in test file path %s."
	       (buffer-file-name)))
    (setq path-elements (split-string path-from-src "/" t))
    (setq dirdepth (length path-elements))
    (setq path-to-src "")
    (while (not (equal dirdepth 1))
      (setq path-to-src (concat "../" path-to-src))
      (setq dirdepth (- dirdepth 1)))

    ;; Test or not test?
    (forward-line 2)
    (if (not (re-search-backward (concat "^\\(}\\|" chrome-test-regexp "\\)")
					 (point-min) t))
	(setq in-test nil))
    (if (not (looking-at chrome-test-regexp))
	(setq in-test nil))

    (if in-test
	(progn
	  ;; Figure out the test name
	  (setq testname
		(concat (buffer-substring (match-beginning 1) (match-end 1))
			"."
			(buffer-substring (match-beginning 2) (match-end 2))))

	  ;; Figure out the test type
	  (setq test_executable_name
		(cond
		 ((and (string-match "_unittest.cc$" filename)
		       (equal (car path-elements) "content"))
		  "content_unittests")
		 ((and (string-match "_browsertest.cc$" filename)
		       (equal (car path-elements) "content"))
		  "content_browsertests")
		 ((and (string-match "_unittest.cc$" filename)
		       (equal (car path-elements) "chrome"))
		  "unit_tests")
		 ((and (string-match "_browsertest.cc$" filename)
		       (equal (car path-elements) "chrome"))
		  "browser_tests")
		 ((and (string-match "_uitest.cc$" filename)
		       (equal (car path-elements) "chrome"))
		  "ui_tests")
		 ((and (string-match "_unittest.cc$" filename)
		       (equal (car path-elements) "net"))
		  "net_unittests")
		 (t nil)))
	  (if (not test_executable_name)
	      (error "Coudln't interpret test name %s." (buffer-file-name)))

	  ;; Compile and run the sucker.
	  (compile (concat "cd " path-to-src
			   "; chrmake " test_executable_name
			   " && out/Debug/" test_executable_name
			   " --gtest_filter=" testname))
	  )
      ;; (not in-test)
      (compile (concat "cd " path-to-src "; chrmake ../../"
		       path-from-src "^"))))))

(defconst chrome-codesearch-search-template
  "https://code.google.com/p/chromium/codesearch#search/&q=%s&sq=package:chromium"
  "URL to visit for searching for a particular string in codesearch.")

(setq chrome-codesearch-history '())

(defun chrome-codesearch (search-string)
  (interactive (list
		(read-from-minibuffer
		 "Tag to Codesearch: " 
		 (randy-word-under-point)
		 nil nil chrome-codesearch-history)))
  (browse-url (format chrome-codesearch-search-template search-string)))

(provide 'chrome-dev)
