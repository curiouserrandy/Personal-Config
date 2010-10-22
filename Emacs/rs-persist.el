;;; Code for persisting emacs variables.

(defconst rs-persist-file
  (concat (getenv "HOME") "/.emacs_persisted_vars")
  "File in which to persist variables.")

(defvar rs-persist-variable-list nil
  "List of variables to save on emacs exit.")

(defun rs-save-variables (varlist file)
  (save-excursion
    (set-buffer (find-file-noselect file))
    ;; Nuke; we're going to regenerate.
    (delete-region (point-min) (point-max))
    (while varlist
      (let ((nextvar (car varlist)))
	(if (not (symbolp nextvar))
		 (error "Entry %s in varlist is not a symbol." nextvar))
	(setq varlist (cdr varlist))
	(insert (rs-varcreate-string nextvar))))
    (save-buffer)))

(defun rs-varcreate-string (var)
  (concat "(setq " (symbol-name var) " (quote "
	  (prin1-to-string (symbol-value var))
	  "))
"))

(defun rs-persist-variable (var)
  (interactive "SSymbol to persist: ")
  (setq rs-persist-variable-list
	(delete-dups (cons var rs-persist-variable-list))))

(add-hook 'kill-emacs-hook
	  (lambda () (rs-save-variables
		      rs-persist-variable-list rs-persist-file)))

(if (file-readable-p rs-persist-file)
    (load-file rs-persist-file))
      
;;; It's hard to imagine a circumstance in which you wouldn't want 
;;; your list of persisted variables persisted.  Obviously, this would
;;; only be needed on the first invocation of this file.
;;; Commented out during testing.
; (if (not (memq 'rs-persist-variable-list rs-persist-variable-list))
;	 (rs-persist-variable 'rs-persist-variable-list))

	
	
;; This file may be working; try it on something hard and then enable.

(provide 'rs-persist)
