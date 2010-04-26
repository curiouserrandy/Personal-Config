;;; "randy-split" is a major mode in which all alphanumeric keys cause
;;; the current chunk of text in the current buffer to be excised and
;;; placed into a buffer named "*randy-split-<key>*".  The cursor is
;;; moved to the beginning of the next chunk of text.

;;; Chunks of text are defined by the variables chunk-start and
;;; chunk-end (maybe these should be the paragraph variables, but my
;;; default is not a paragraph default).  By default these variables
;;; point to functions that specify the beginning of the current line
;;; and the beginning of the next line, so these functions will
;;; operate on lines.  It should be possible to do variants that work
;;; on diff chunks & etc.

;;; Todo:
;;;	Setup auto move forward to next interesting line (beginning of
;;;	chunk or known broadcast heading)
;;;
;;;	Setup keys to move to next or previous broadcast heading.

(defun randy-split-line-start ()
  (save-excursion
    (forward-line 0)
    (point)))

(defun randy-split-line-end ()
  (save-excursion
    (forward-line 1)
    (point)))

(defvar randy-split-chunk-start 'randy-split-line-start
  "Function to call to return the start of the current text chunk.")

(defvar randy-split-chunk-end 'randy-split-line-end
  "Function to call to return the start of the current text chunk.")

(defvar randy-split-submode-name "Line"
  "Name of split submode currently active.")

(defvar randy-split-current-heading ""
  "Heading to be used to start out new buffers")

(defun randy-split-broadcast-heading ()
  "Replicate the current line in all currently existing split buffers.
Also set it to be used as the new heading for any new buffers.
Does not delete it from the current buffer"
  (interactive)
  (let ((heading-contents
	 (buffer-substring
	  (save-excursion (forward-line 0) (point))
	  (save-excursion (forward-line 1) (point))))
	(all-buffers (buffer-list))
	current-buffer)
    (while all-buffers
      (setq current-buffer (car all-buffers))
      (setq all-buffers (cdr all-buffers))
      (and (string-match "^\\*randy-split" (buffer-name current-buffer))
	   (progn
	     (set-buffer current-buffer)
	     (goto-char (point-max))
	     (insert heading-contents))))
    (setq randy-split-current-heading heading-contents)
    (message "Split broadcast heading set to: %s" heading-contents)))

(defun randy-split-null-heading ()
  "Set the current heading to null."
  (interactive)
  (setq randy-split-current-heading nil)
  (message "Split broadcast heading nulled."))

(defun randy-split-chunk-excise (buffer-suffix)
  "Move the current text chunk from the current buffer to buffer *randy-split-BUFFER-SUFFIX*.
The current text chunk is defined by the variables
RANDY-SPLIT-LINE-START and RANDY-SPLIT-LINE-END.
Screen display is unchanged; the cursor is moved
to the first character after the end of this text chunk."
  (let ((target-buffer (concat "*randy-split-" buffer-suffix "*"))
	(chunk-start (funcall randy-split-chunk-start))
	(chunk-end (funcall randy-split-chunk-end))
	chunk-contents buffer-existed)
    (save-excursion
      (setq chunk-contents (buffer-substring chunk-start chunk-end))
      (delete-region chunk-start chunk-end)
      (setq buffer-existed (get-buffer target-buffer))
      (set-buffer (get-buffer-create target-buffer))
      (goto-char (point-max))
      (or buffer-existed
	  (insert randy-split-current-heading))
      (insert chunk-contents))))

(defun randy-split-move-chunk (arg)
  "Move the current chunk of text into a different buffer."
  (interactive "p")
  (randy-split-chunk-excise
   (char-to-string last-command-event))) ;Should always be a character
					 ;here. 

(defvar randy-split-mode-old-local-map)
(defvar randy-split-mode-old-mode-name)
(defvar randy-split-mode-old-major-mode)

(defconst randy-split-mode-map nil)

(defun picture-substitute (oldfun newfun)
  (substitute-key-definition oldfun newfun randy-split-mode-map global-map))

(if (not randy-split-mode-map)
    (progn
      (setq randy-split-mode-map (list 'keymap (make-vector 256 nil)))
      (picture-substitute 'self-insert-command 'randy-split-move-chunk) 

      (define-key randy-split-mode-map "\C-c\C-c" 'randy-split-mode-exit)
      (define-key randy-split-mode-map "\C-c\C-b" 'randy-split-broadcast-heading)
      (define-key randy-split-mode-map "\C-c\C-n" 'randy-split-null-heading)
      (define-key randy-split-mode-map "\C-cd" 'randy-split-diff-submode)
      (define-key randy-split-mode-map "\C-cc" 'randy-split-cdiff-submode)
      (define-key randy-split-mode-map "\C-cl" 'randy-split-line-submode)))

(defun randy-split-set-mode-name ()
  (setq mode-name (concat "Randy-Split:" randy-split-submode-name))
  (force-mode-line-update))

(defun randy-split-mode ()
  "Switch to randy-split mode, used for interactively assigning text chunks to buffers."
  (interactive)
  (if (eq major-mode 'randy-split-mode)
      (error "You are already in split mode.")
    (make-local-variable 'randy-split-mode-old-local-map)
    (setq randy-split-mode-old-local-map (current-local-map))
    (use-local-map randy-split-mode-map) ;; ***
    (make-local-variable 'randy-split-mode-old-mode-name)
    (setq randy-split-mode-old-mode-name mode-name)
    (make-local-variable 'randy-split-mode-old-major-mode)
    (setq randy-split-mode-old-major-mode major-mode)
    (make-variable-buffer-local 'randy-split-submode-name)
    (setq major-mode 'randy-split-mode)
    (randy-split-set-mode-name)

    (message "Type %s in this buffer to return it to %s mode."
	     (substitute-command-keys "\\[randy-split-mode-exit]")
	     randy-split-mode-old-mode-name)))

(defun randy-split-mode-exit ()
  "Undo randy-split-mode and return to previous major mode."
  (interactive)
  (if (not (eq major-mode 'randy-split-mode))
      (error "You aren't in split mode.")
    (setq mode-name randy-split-mode-old-mode-name)
    (use-local-map randy-split-mode-old-local-map)
    (setq major-mode randy-split-mode-old-major-mode)
    (force-mode-line-update)))
    
;;; A series of alternative definitions for randy-split-chunk-start
;;; and randy-split-chunk-end. 

(defconst randy-split-diff-re
  "^[0-9][0-9]*\\(,[0-9][0-9]*\\)?[adc][0-9][0-9]*\\(,[0-9][0-9]*\\)?$"
  "Regular expression to mark beginning of non-context diffs.")

(defun randy-split-diff-start ()
  (save-excursion
    (end-of-line)
    (re-search-backward randy-split-diff-re) ;We want error if we
					     ;don't find it.
    (point)))

(defun randy-split-diff-end ()
  (save-excursion
    (let ((end-re (concat randy-split-diff-re
			  "\\|^Index:\\|^Common subdirectories:\\|^diff ")))
      (forward-line 1)
      (re-search-forward end-re (point-max) 1)
      (beginning-of-line)
      (point))))

(defconst randy-split-cdiff-re
  "^\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*$"
  "Regular expression to mark beginning of non-context diffs.")

(defun randy-split-cdiff-start ()
  (let ((randy-split-diff-re randy-split-cdiff-re))
    (randy-split-diff-start)))

(defun randy-split-cdiff-end ()
  (let ((randy-split-diff-re randy-split-cdiff-re))
    (randy-split-diff-end)))

(defun randy-split-diff-submode ()
  (interactive)
  (if (not (eq major-mode 'randy-split-mode))
      (error "You aren't in split mode.")
    (setq randy-split-submode-name "Diff")
    (setq randy-split-chunk-start 'randy-split-diff-start)
    (setq randy-split-chunk-end 'randy-split-diff-end)
    (randy-split-set-mode-name)))

(defun randy-split-cdiff-submode ()
  (interactive)
  (if (not (eq major-mode 'randy-split-mode))
      (error "You aren't in split mode.")
    (setq randy-split-submode-name "CtxDiff")
    (setq randy-split-chunk-start 'randy-split-cdiff-start)
    (setq randy-split-chunk-end 'randy-split-cdiff-end)
    (randy-split-set-mode-name)))

(defun randy-split-line-submode ()
  (interactive)
  (if (not (eq major-mode 'randy-split-mode))
      (error "You aren't in split mode.")
    (setq randy-split-submode-name "Line")
    (setq randy-split-chunk-start 'randy-split-line-start)
    (setq randy-split-chunk-end 'randy-split-line-end)
    (randy-split-set-mode-name)))




