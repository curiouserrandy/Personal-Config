;;; Create a unique name for the buffer
(defun randy-uniquename-buffer (suffix)
  (interactive "sBuffer suffix (default: remove suffix): ")
  (if (equal suffix "")
      (setq suffix nil))
  (let ((bname (buffer-name)) mname csuffix)
    (if (not (let ((case-fold-search nil))
	       (string-match "^\\([^<>]*\\)\\(<[^<>]*>\\)?$" bname)))
	(message (concat "Buffer name " bname " not in proper format"))
      (setq mname (substring bname (match-beginning 1) (match-end 1)))
      (if suffix
	  (rename-buffer (concat mname "<" suffix ">"))
	(rename-buffer mname)))))

(provide 'rs-buffernames)
