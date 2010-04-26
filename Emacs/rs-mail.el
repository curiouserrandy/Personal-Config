
(defun randy-scrub-html ()
  "Remove HTML from buffer."
  (interactive)
  (save-excursion
    (replace-regexp "<[^>]*>" "" nil (point-min) (point-max))
    (replace-string "&nbsp;" " " nil (point-min) (point-max))))

