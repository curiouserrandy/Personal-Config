(require 'rs-grepplusplus)

(defun git-top-directory (dir)
  "Return the most recent ancestor of dir with a .git subdirectory in it."
  (let (lastdir)
    (while (not (or (equal dir lastdir)
		    (file-directory-p (concat dir "/.git"))))
      (setq lastdir dir)
      (setq dir (file-name-directory (directory-file-name dir))))
    (and (file-directory-p (concat dir "/.git")) dir)))

;; Only provide these capabilities if git is present on the system.
(if (executable-find "git")
    (progn 

      (defvar gitgrep-history nil "History variable for git grep runs.")

	(defun randy-run-git-grep ()
	  (interactive)
	  (let ((grep-null-device "") (null-device "") git-dir)
	    (setq git-dir (git-top-directory default-directory))
	    (if (not git-dir) (error "Not in a git tree."))
	    (let ((default-directory git-dir))
	      (grep-like-command "git grep -nw" t 'gitgrep-history)
	      (set-buffer "*grep*")
	      (cd git-dir))))

	(provide 'rs-tools-git)))