;;; Notes:
;;;	* Not using fullscreen . because it removes the resize thumbnail,
;;;	  and automatically and incorrectly takes into account the menu bar
;;;	

;;; ???: Should I be doing anything with defaulting visibility, iconification, or similar?
;;; ???: Ditto on letting width/height overright fullscreen . ?

;;; Use case: I want to have a couple of configurations that I can
;;; reach with keystrokes.  In an ideal world, I'd have configs that
;;; defaulted based on screen size.  Specifically, when I had a config
;;; I was happy with, I'd do something to save it, and it would be
;;; available for that screen size (only).  The pieces of this are:
;;;	* Way to get current configuration
;;;	* Format for storing lists of configurations.
;;;	* Persisted variable in which to save it.  That variable would
;;;	  be first indexed by screen size, and then by name.  First
;;; 	  element in the screen size list would be the default.
;;;	* Keystroke bound to "set frame config" that would prompt the 
;;;	  user for the config wanted, and force it.  

;;; A frame config looks like a list of alists of frame properties.  
;;; A frame config list looks like:
;;;	((<display-size> ((<name> frame-config) ...)) ...)
;;; The first element in a display size list is the default.
;;; Function that sets emacs frame config:
;;;	* interactive
;;;	* Takes a name as argument
;;;	* Completes based on names in list
;;; Function that creates list:
;;;	* Prompts for name; refuses to save without name
;;;	* Checks for conflicts; prompts if overwrite
;;; Separate function "randy-set-default-config":
;;;	* Sets the current config as default
;;;	* Prompts for a name if it isn't already in the list

;;; XXX: REgularize names and make distinctionb etween saving and restoring. 

;;; You know, this would all be simpler if you had just made the name "default" 
;;; special and not worried about the order :-J.

(defconst randy-interesting-frame-properties '(top left width height)
  "Properties to include in auto-generated frame configs.")

(defvar randy-frame-configs-list nil
  "List of possible configs for emacs windows.")

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
  (mapcar '(lambda (fc) 
	     ;; Pull out just the interesting values
	     (mapcar '(lambda (prop) (assoc prop fc))
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
	 (cdr (assoc config randy-frame-configs-list))))
    (if (not (assoc name (randy-current-display-list)))
	(error "Couldn't find config for name " name))
    (sort (randy-current-display-list)
	  ;; name is < everything; everything else is equal
	  '(lambda (a b) (equal (car a) name)))))

;; UI Functions 
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


(provide 'rs-frames)
