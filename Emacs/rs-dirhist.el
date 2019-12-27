(require 'rs-persist)
(require 'dired)

(defvar randy-dired-history-size 8 "Number of dired visited directories kept")
(defvar randy-dired-history nil "History of directories visited through dired")

(rs-persist-variable 'randy-dired-history)

(defun randy-rotate-dired-history ()
  "Rotate the dired history, moving the directory at the top to the bottom
and visiting the next directory through dired.
No effect if history is null."
  (interactive)
  (if (not randy-dired-history)
      (message "No dired history to rotate")
    (setq randy-dired-history (append (cdr randy-dired-history)
				      (list (car randy-dired-history))))
    (find-file (car randy-dired-history))))

(defun randy-dired-history-save ()
  "Save the current directory at the front of the history list
and snip list at end if needed.
Does not change the list if the current directory is already
at the front of the list."
  (if (equal default-directory (car randy-dired-history))
      nil
    (setq randy-dired-history (cons default-directory randy-dired-history))
    (if (> (length randy-dired-history) randy-dired-history-size)
	(setq randy-dired-history (butlast randy-dired-history)))))

(defun randy-visit-last-directory ()
  (if (not randy-dired-history)
      (message "No directory to visit")
    (find-file (car randy-dired-history))))

(add-hook 'dired-mode-hook 'randy-dired-history-save)

(provide 'rs-dirhist)
