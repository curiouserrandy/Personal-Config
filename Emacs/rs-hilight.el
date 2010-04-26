(defun randy-highlight-word (name)
  "Hilight all instances of NAME in the current buffer.
With a prefix argument, highlight them even as part of a larger word."
  (interactive "sIdentifier to highlight: ")
  (let ((highlight-regexp (concat "\\b" (regexp-quote name) "\\b"))
	(mod (buffer-modified-p))
	(ro buffer-read-only))
    (save-excursion
      (if ro (toggle-read-only))
      (goto-char (point-min))
      (while (re-search-forward highlight-regexp (point-max) t)
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(face highlight)))
      (set-buffer-modified-p mod)
      (if ro (toggle-read-only)))))

(defun randy-remove-highlighting ()
  "Remove all highlighting within the buffer."
  (interactive)
  (let ((mod (buffer-modified-p))
	(ro buffer-read-only))
    (if ro (toggle-read-only))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (set-buffer-modified-p mod)
    (if ro (toggle-read-only))))

(provide 'rs-hilight)

