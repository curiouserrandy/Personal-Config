;;; Function to add a newline and indent to current level
;;; Should be modified so that auto fill does the same thing. XXX
(defun vi-type-indent ()
  "Add a newline and indent to level of current line"
  (interactive)
  (let ((cindent (current-indentation)))
    (insert ?\n)
    (indent-to cindent)))

;;; Function to bind the previous function to nl
;;; Should save old indentation and restore when asked.  XXX
(defun set-vi-type-indent ()
  "Make LF key indent vi style"
  (interactive)
  (local-set-key "\n" 'vi-type-indent)
  (message "Vi style indent active"))

(provide 'rs-vi)
