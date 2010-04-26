(defun randy-wiki-prep ()
  "Fix a buffer using simplified wiki markup.
Changes a buffer using an altered wiki markup language in which
list item/indentation tags remain in effect until the next
paragraph break into one that uses the documented wiki markup.  This is
done by converting paragraphs that should all be using the above
wiki markup into very long lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\([*#:;\'].*\\)\\(\n[ 	]+[^ 	\n].*\\|\n[^ 	\n*#:;\'].*\\)+\n"
	    (point-max) t)
      (let ((beg (match-beginning 0)) (end (1- (match-end 0))))
	(goto-char beg)
	(while (search-forward "\n" end t)
	  (replace-match " "))))))


(provide 'rs-wiki)
