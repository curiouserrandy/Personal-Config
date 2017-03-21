;;; Easy configuration of emacs windows on a per-screen, per-display basis.

;;; Debugging guide:
;;;	(randy-current-display-config)
;;; 	(randy-current-display-list)
;;;	randy-frame-configs-list

(require 'rs-persist)

(defconst randy-interesting-frame-properties '(top left width height)
  "Properties to include in auto-generated frame configs.")

(defvar randy-frame-configs-list nil
  "List of possible configs for emacs windows.")
(rs-persist-variable 'randy-frame-configs-list)

(defun randy-current-display-config ()
  "List identifying display."
  (list (display-pixel-width) (display-pixel-height)))

(defun randy-current-display-list ()
  "List of configurations associated with current display."
   (cdr (assoc (randy-current-display-config) randy-frame-configs-list)))

(defun randy-get-frame-config ()
  "Return the current configuration of the emacs frames as a frame config.
Only include in it frame properties in randy-interesting-frame-properties."
  ;; For each set of parameters
  (mapcar #'(lambda (fc) 
	     ;; Pull out just the interesting values
	     (mapcar #'(lambda (prop) (assoc prop fc))
		     randy-interesting-frame-properties))
	  (mapcar 'frame-parameters (frame-list))))
  
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

(defun randy-save-frame-config (name config &optional display-id)
  "Save the passed config in name NAME under display id DISPLAY-ID 
 (defaults to current).  This will overwrite any
configuration previously saved under that name."
  ;; Default display id
  (if (not display-id)
      (setq display-id (randy-current-display-config)))
  ;; Put configs for display-id in known state
  (if (not (assoc display-id randy-frame-configs-list))
      (setq randy-frame-configs-list
	    (append randy-frame-configs-list (list (list display-id)))))
  (let ((display-entry (assoc display-id randy-frame-configs-list)))
    ;; Make sure there's an entry for the name
    (if (not (assoc name (cdr display-entry)))
	(setcdr display-entry
		(append (cdr display-entry) (list (list name)))))
    ;; Overwrite whatever was there before
    (setcdr (assoc name (cdr display-entry)) config)
    (message "Config \"%s\" saved." name)))

(defun randy-make-named-config-default (name &optional display-id)
  "Make the config of the given name default.  Illegal if it isn't in
the current displays list of names."
  (if (not display-id) (setq display-id (randy-current-display-config)))
  (let ((current-display-config-list
	 (cdr (assoc display-id randy-frame-configs-list))))
    (if (not (assoc name (randy-current-display-list)))
	(error "Couldn't find config for name %s" name))
    (sort (randy-current-display-list)
	  ;; name is < everything; everything else is equal
	  #'(lambda (a b) (equal (car a) name)))))

;;; UI Functions 
(defun randy-save-current-config (non-default)
  "Save the current configuration.  Without a prefix argument, save it as the
default configuration with a name of \"default\".  With a prefix arg, 
prompt for the name of the configuration and save using that name.
The results of saving with a name of \"default\" are undefined."
  (interactive "P")
  (let ((name "default") (config (randy-get-frame-config)))
    (if non-default
	(setq name (completing-read "Configuration name: "
		     (mapcar 'car (randy-current-display-list)))))
    (if (and (assoc name (randy-current-display-list))
	     (not (y-or-n-p (format "Overwrite config \"%s\"? " name))))
	(error "Config %s already exists." name))
    (randy-save-frame-config name config)
    (if (not non-default)
	(randy-make-named-config-default name))))

(defun randy-restore-config (name)
  "Restore the default config for this display.  With prefix arg, prompt
for a named config."
  (interactive
   (list
    (if current-prefix-arg 
	(completing-read "Configuration to restore: "
			 (mapcar 'car (randy-current-display-list)) nil t)
      (car (car (randy-current-display-list))))))
   (randy-force-frame-config (cdr (assoc name (randy-current-display-list)))))

(defun randy-delete-named-config (name &optional display-id)
  "Delete a previous saved config."
  (interactive
   (list 
    (completing-read "Configuration to restore: "
		     (mapcar 'car (randy-current-display-list)) nil t)))
  (if (not display-id) (setq display-id (randy-current-display-config)))
  (let* ((display-entry (assoc display-id randy-frame-configs-list))
	 (key (car (assoc name (cdr display-entry)))))
    (if key				;Handle non-existence name
	(setcdr display-entry (assq-delete-all key (cdr display-entry))))))



;;; Startup.  
;;; If there's a default config for the current display, restore it
(let ((display-entry
       (assoc (randy-current-display-config) randy-frame-configs-list)))
  (if (car (cdr display-entry))		;Checking for list of configs
      (randy-restore-config (car (car (cdr display-entry))))))

(provide 'rs-frames)
