(require 'rs-grepplusplus)
(require 'rs-frames)

(defvar randy-sandbox-directory nil
  "Top of Sandbox tree currently being worked on.
All commands that reference this should be sensitive to in-session
changes to it.
Should always have a trailing '/' (emacs canonical directory)")

(defvar randy-sandboxen-directory nil
  "Directory into which sandboxes are placed.")

(defun randy-change-default-sandbox (sandboxdir &optional dontstartshell)
  (interactive "DNew sandbox directory: ")
  (setq randy-sandbox-directory
	(file-name-as-directory (expand-file-name sandboxdir)))
  ;;; (setq glimpse-base-directory randy-sandbox-directory)
  ;;; Null out histories; they don't really apply between sandboxes.
  (setq glimpse-history nil)
  (setq egrep-history nil)
  (setq fsfile-history nil)
  ;;
  (let ((tags-add-tables nil) (large-file-warning-threshold 50000000))
    (visit-tags-table
     (concat randy-sandbox-directory "/TAGS")))

  (find-file sandboxdir)

  ;; If we're using git, figure out the local branch.
  (let (frame-name)
    (string-match "\\([^/][^/]*?\\)\\(/src\\)?/?$" sandboxdir)
    (setq frame-name (substring sandboxdir
				(match-beginning 1)
				(match-end 1)))
    (if (fboundp 'git-local-branch)
	(setq frame-name (concat frame-name " - " (git-local-branch))))
    (set-frame-name frame-name))
  
  (delete-other-windows)
  (if (not dontstartshell)
      (progn
	(split-window-vertically)
	(other-window 1)
	(shell))))

(defun randy-greplist-sandbox-file (filename)
  "Grep through the list of files included in this sandbox for ones
that match FILENAME as a regexp.  Display the result in the *grep*
buffer in a form that the NEXT-ERROR function can parse."
  (interactive "sFile to Visit: ")
  (if (not randy-sandbox-directory)
      (error "No sandbox currently active."))
  (let* ((grep-highlight-matches nil)
	 (tmpx (grep-compute-defaults))
	 (grep-use-null-device nil) )
    (grep (concat "fsfile -f -s " randy-sandbox-directory " " filename))))

(provide 'rs-sandbox)
