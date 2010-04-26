;;; Notes:
;;;	* Not using fullscreen . because it removes the resize thumbnail,
;;;	  and automatically and incorrectly takes into account the menu bar
;;;	

;;; ???: Should I be doing anything with defaulting visibility, iconification, or similar?
;;; ???: Ditto on letting width/height overright fullscreen . ?
(defun randy-force-frame-config (frames-properties-list)
  "Force the current configuration of frames to look like FRAME-LIST.
FRAME-LIST is a list of alists of frame propreties.  Frames will be deleted or
added to match the number of alists passed in, then each frame configured to
look like one of the alists."
  (let ((icount (length frames-properties-list))
	(aframe (selected-frame))
	(nextframe (next-frame (selected-frame))))
    (while (< icount (length (frame-list)))
      (setq aframe nextframe)
      (setq nextframe (next-frame (selected-frame)))
      (delete-frame aframe))
    (while (> icount (length (frame-list)))
      (make-frame))
    (setq aframe (selected-frame))
    (while frames-properties-list
      (let ((fp (car frames-properties-list)))
	(setq nextframe (next-frame aframe))
	(setq frames-properties-list (cdr frames-properties-list))
	(modify-frame-parameters aframe fp)
	(setq aframe nextframe)
	))))

;;; XXX: Assuming (frame-char-height) is a constant, rather than per-frame
(defun randy-twoframe-config ()
  "Create the frame configuration Randy usually wants to work in.  This is
dependent on screen layout."
  (interactive)
  (let ((display-size (list (x-display-pixel-width) (x-display-pixel-height))))
    (cond
     ((equal display-size '(1440 852))
      ;; Just my standard mac screen
      (randy-force-frame-config
       '(((top . 22) (left . 0) (width . 80) (height . 48))
	 ((top . 22) (left . -1) (width . 80) (height . 48)))))
     ((equal display-size '(2720 1024))
      ;; Home dual monitors, 1440x852 + 1280x1024
      (randy-force-frame-config
       '(((top . 0) (left . 1440) (width . 80) (height . 60))
	 ((top . 0) (left . -1) (width . 80) (height . 60)))))
     (t
      (error "Unknow display configuration; reported size: %S"
	     display-size)))))
    

(defun second-frame (&optional name-string)
  "Create a second frame at the right edge of the screen.
Optional title it with NAME-STRING."
  (let ((fm
	 (make-frame '((left . -1) (width . 80) (height . 50)
		       (user-position . t) (font . "7x14")))))
    (if name-string
	(save-excursion
	  (select-frame fm)
	  (randy-set-frame-name name-string)))))

(defun randy-set-frame-name (&optional tagline)
  "Set the name for the current emacs frame"
  (interactive)
  (if (not tagline)
      (setq tagline "misc"))
  (let* ((sysname (system-name))
	 (first-period (progn
		  	 (and (string-match "^\\([^\\.]*\\)\\." (system-name))
			      (match-end 1))))
	 (hostname (if first-period (substring sysname 0 first-period) sysname)))
    (set-frame-name
     (concat (user-login-name) "@"
	     hostname "(" tagline ")"))))

(provide 'rs-frames)

