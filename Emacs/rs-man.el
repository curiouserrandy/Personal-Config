;;; Function to replace current buffer with the result of that buffer
;;; run through "nroff -man" and pretified.
(defun randy-format-man-page ()
  (interactive)
  (let ((ro buffer-read-only))
    (save-excursion
      (if ro (toggle-read-only))
      (shell-command-on-region (point-min) (point-max) "nroff -man"  t)
      (goto-char (point-min))
      (replace-regexp "." "")
      (not-modified)
      (if ro (toggle-read-only)))))

(provide 'rs-man)

