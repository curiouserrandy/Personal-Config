(require 'rs-grepplusplus)

(defun git-top-directory (dir)
  "Return the most recent ancestor of dir that is the top of a git tree.
This is tested for by checking for .git, either as a repo or a file."
  (let (lastdir)
    (while (not (or (equal dir lastdir)
		    (file-readable-p (concat dir "/.git"))))
      (setq lastdir dir)
      (setq dir (file-name-directory (directory-file-name dir))))
    (and (file-readable-p (concat dir "/.git")) dir)))

;; Only provide these capabilities if git is present on the system.
(if (executable-find "git")
    (progn 

      (defvar gitgrep-history nil "History variable for git grep runs.")

	(defun randy-run-git-grep ()
	  (interactive)
	  (let ((grep-null-device nil) (null-device "") git-dir)
	    (setq git-dir (git-top-directory default-directory))
	    (if (not git-dir) (error "Not in a git tree."))
	    (let ((default-directory git-dir))
	      (grep-like-command "git grep -nw" t 'gitgrep-history)
	      (set-buffer "*grep*")
	      (cd git-dir))))

	(provide 'rs-tools-git)))

(require 'dired)
(require 'vc-git)

(defun git-tree-dirty-p ()
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (call-process "git" nil '(t nil) nil "status" "-s")))))
    (string-match "^\\([^?]\\|\\?[^?]\\)" str)))

(defun git-local-branch () 
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (call-process "git" nil '(t nil) nil "symbolic-ref" "HEAD")))))
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
	(match-string 2 str)
      str)))

(defvar randy-vc-set-with-hack nil
  "Indicate whether the vc line for a dired buffer is being set via
the hacky dired-after-readin-hook method.")

(make-variable-buffer-local 'randy-vc-set-with-hack)
(setq-default randy-vc-set-with-hack nil)

(add-hook 'dired-after-readin-hook
	  (lambda ()
	    (if (or (not vc-mode) randy-vc-set-with-hack)
		(progn
		  (setq vc-mode
			(let ((branch (git-local-branch))
			      (dirty (git-tree-dirty-p)))
			  (concat "Git" (if dirty ":" "-") branch)))
		  (setq randy-vc-set-with-hack t)
		  (force-mode-line-update)))))

(provide 'rs-git)
