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
  (mapcar 
   '(lambda (fc) 
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

;;; Organizing and renaming after this line.

;;; XXX: Todo: put at end unless default is set, then at beginning.
(defun randy-save-frame-config (name &optional default config)
  "Save the current configuration in name NAME.
Optional argument DEFAULT if non-nil indicates to make this the 
default configuration (set interactively via prefix arg).  Optional
argument CONFIG if non-nil means use the specified configuration rather
than taking it from the current config."
  ;; I want the name to be prompted for with symbol completions what's in the
  ;; current alist.  default is just the prefix arg.
  (interactive "MSave configuration name (default \"default\"): \nP\ni")
  (let* ((display-id (list (display-pixel-width) (display-pixel-height)))
	 (config-names
	  (mapcar 'car 
		  (cdr (assoc display-id randy-frame-configs-list)))))
    (if (equal name "") (setq name "default"))
    (if (not config)
	(setq config (randy-get-frame-config)))
    (if (or (not (called-interactively-p))
	    (not (member name config-names))
	    (y-or-n-p "Name %s will overwrite existing config; continue?"))
	(if (assoc display-id randy-frame-configs-list)
	    (let ((old-list (cdr (assoc display-id randy-frame-configs-list))))
	      (setcdr (assoc display-id randy-frame-configs-list)
		      (cons (cons name config) old-list)))
	  (setq randy-frame-configs-list
		(cons (list display-id (cons name config))
		      randy-frame-configs-list)))))
  name)

(defun randy-set-saved-frame-config (name)
  "Specify a saved frame configuration by name.  Default is first element
in list."
  (interactive
   (list
    (read-from-minibuffer
     (if (randy-current-display-list)
	 (concat "Configuration name (default "
		 (caar (randy-current-display-list)) " ): ")
       "Configuration name (no default): "))))
  (if (not name)
      (setq name (caadr (randy-current-display-list))))
  (if (not name)
      (error "No configs exist for display config "
	     (randy-current-display-config)))
  (let ((config (cdr (assoc name 
			   (randy-current-display-list)))))
    (if (not config)
	(error "Couldn't find config for name \"%s\"" name))
    (randy-force-frame-config config))
  name)

(defun randy-make-named-config-default (name)
  "Make the config of the given name default.  Illegal if it isn't in
the current displays list of names."
  (let ((current-display-config-list
	 (cdr (assoc (randy-current-display-config)
		     randy-frame-configs-list))))
    (if (not (assoc name (randy-current-display-list)))
	(error "Couldn't find config for name " name))
    (sort (randy-current-display-list)
	  ;; name is < everything; everything else is equal
	  '(lambda (a b) (equal (car a) name)))))

(defun randy-make-current-config-default ()
  (interactive)
  ;;; Figure out if the current config is in the list.
  ;;; If it's not, create it.  
  ;;; Regardless, move it to the front
  (let* ((current-display (randy-current-display-config))
	 (frame-config (randy-get-frame-config))
	 (display-context (assoc current-display randy-frame-configs-list))
	 (frame-config-name (car (rassoc frame-config (cdr display-context)))))
    (if (not frame-config-name)
	(setq frame-config-name
	      (call-interactively 'randy-save-frame-config)))
    (if (not frame-config-name)
	(error "\"Can't happen\": frame-config-name not specified after randy-set-saved-frame-config"))
    (randy-make-named-config-default frame-config-name)))

;;; After this line not part of generic samed config stuff.

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

