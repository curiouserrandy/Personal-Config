
(require 'rs-underpoint)
(require 'cc-mode)

(defconst randy-gprof-find-tag-syntab
  (let ((syntab (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" syntab)
    syntab)
  "Syntax table to use for distinguishing C functions in text.")


(defun randy-gprof-find-tag (tag)
  "Go to the analysis of the routine TAG in the current file
   (assumes the current file is a gprof file)."
  (interactive (list
		(let ((defrtn (randy-word-under-point randy-gprof-find-tag-syntab)))
		  (read-string (concat "Routine name (" defrtn "): ")
			       nil nil defrtn))))
  (push-mark (point))
  (goto-char (point-min))
  (re-search-forward (concat "^\\[.*\\_<" (regexp-quote tag) "\\_>")))

