;;; My customizations for mh.

(setq mh-delete-yanked-msg-window t)	; Yanking a message blows the window
					; from which the message was pulled.

(setq mh-reply-default-reply-to "all")	; By default, reply to all.

(setq mh-recursive-folders t)		; Enable recursive folders

;;; Sorry Stephen, this is the functionality I want.  The problem is that
;;; you can't visit a folder from outside of mh without interacting with
;;; the user, and I'm not using the 'inc' functionality of mh, which means
;;; that going in by way of a naked mh-rmail gives an error.
(autoload 'mh-visit-folder "mh-e"
  "Visit FOLDER and display RANGE of messages.
Do not call this function from outside mh-e; see \\[mh-rmail] instead." t)

(autoload 'mh-find-path "mh-e")		;mh-e loads mh-utils, which is
					;where it really is.
(defun randy-enter-mh ()
  (interactive)
  (mh-find-path)
  (mh-visit-folder "+default" "all"))


(eval-after-load "mh-comp"
  '(progn
     (defun mh-smail-batch (&optional to subject other-headers &rest ignored)
       "Set up a mail composition draft with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.  This function does not prompt the user
for any header fields, and thus is suitable for use by programs
that want to create a mail buffer.
Users should use `\\[mh-smail]' to compose mail."
       (interactive)
       (mh-find-path)
       (let ((mh-error-if-no-draft t))
	 (mh-send (or to "") "" (or subject ""))
	 (goto-char (point-min))
	 (end-of-line)))))

(eval-after-load "mh-e"
  '(progn (if (string-match "^GNU Emacs 19\\." (emacs-version))
	      (progn
		(define-key global-map "\C-xm" 'mh-smail-batch)
		(define-key mh-folder-mode-map "m" 'mh-smail-batch)
		(define-key mh-folder-mode-map "s" 'mh-smail-batch))
	    (progn
	      (define-key mh-folder-mode-map "m" 'compose-mail)
	      (define-key mh-folder-mode-map "s" 'compose-mail)))
	  (define-key mh-folder-mode-map "z" (make-keymap))
	  (define-key mh-folder-mode-map "zf" 'randy-mh-sort-sender)
	  (define-key mh-folder-mode-map "zt" 'randy-mh-sort-recipient)
	  (define-key mh-folder-mode-map "zs" 'randy-mh-sort-subject)
	  (define-key mh-folder-mode-map "zd" 'randy-mh-sort-date)))

;;; When I do send letters in mh letter mode, make sure that the formatting
;;; setup is the way I like it.
(add-hook 'mh-letter-mode-hook '(lambda ()
				 (setq fill-prefix "  ")
				 (define-key mh-letter-mode-map
				   "\C-c\C-r" 'randy-include-signature)))
				   

;;; Some functions to sort by various.

(defun randy-mh-sort-sender ()
  (interactive)
  (let ((mh-sortm-args '("-textfield" "from" "-limit" "0")))
    (mh-sort-folder t)))

(defun randy-mh-sort-recipient ()
  (interactive)
  (let ((mh-sortm-args '("-textfield" "to" "-limit" "0")))
    (mh-sort-folder t)))

(defun randy-mh-sort-date ()
  (interactive)
  (mh-sort-folder))

(defun randy-mh-sort-subject () 
  (interactive)
  (let ((mh-sortm-args '("-textfield" "subject" "-limit" "0")))
    (mh-sort-folder t)))

;;; Currently garbage.
(defun randy-mh-make-folder-keymap ()
  (let* ((menu-keymap (make-sparse-keymap "MH Folders"))
	 folders-output)
    (save-excursion
      (set-buffer "mh scratch buffer")
      (call-process  "folders" nil t nil "-all -fast -recurse")
      )))

    
;;; Fcc stuff.

(defun randy-fcc-sending-mail (output-folder)
  (interactive "sFolder to file: ")
  (goto-char (point-min))
  (re-search-forward (concat "^" mail-header-separator "$"))
  (beginning-of-line)
  (insert "Fcc: " output-folder "
X-Randy-Fccd: " output-folder "
"))

(eval-after-load "mh-comp"
  '(define-key mh-letter-mode-map "\C-cx" 'randy-fcc-sending-mail))
