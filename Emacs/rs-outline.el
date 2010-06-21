(add-hook 'outline-mode-hook 'hide-body)

(defvar outline-explode nil 
  "List of headings to explode in outline mode.
Interpretted as regular expressions anchored after initial *'s & space.")

(defun randy-explode-hook ()
  "Explode items mentioned in EXPLODE variable, or ending in a \"+\"."
  (if (derived-mode-p 'outline-mode)
      (let ((explode-re
	     (if outline-explode
		 (concat "^\\*\\**[ 	]*" outline-explode "\\|^\\*.*\\+$")
	       "^\\*.*\\+$")))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward explode-re (point-max) t)
	    (show-children)
	    (show-entry))))))

(defun randy-implode-hook ()
  "Hide subtrees ending in a \"-\"."
  (if (derived-mode-p 'outline-mode)
      (let ((implode-re "^\\*.*\\-$"))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward implode-re (point-max) t)
	    (hide-subtree)
	    (hide-entry))))))

;;; Pretty much any value should be safe for outline-explode; it doesn't
;;; matter what entries are open and closed upon reading.
;;; Hope I'm right on that :-}.
(put 'outline-explode 'safe-local-variable 'stringp)

(add-hook 'hack-local-variables-hook 'randy-explode-hook t)
(add-hook 'hack-local-variables-hook 'randy-implode-hook t)

(provide 'rs-outline)
