;;; TODO:
;;;	*) Ability to read this file from a different account.
;;;	*) Move all machine specific stuff out to a separate file.
;;;	*) Directory changing command.
;;;	*) Menu of tags tables
;;;	*) Grep in a dired should cd to the current (position wise)
;;;	   subdirectory before executing.
;;;	*) C-p and C-n in rmail summary not changing the rmail buffer.
;;;	*) Function to find a file wherever it is in the backing list.
;;;	*) Function to visit file under point (in other window and in
;;;	   current window).
;;;	*) Save RMAIL file if idle for more than one hour.  Kill buffer if
;;;	   idle for more than two hours(?)
;;;	*) Get C language auto-filling to work the way you'd like.
;;;	*) Way to set window boundaries based on the mouse?  (May already
;;;	   exist).
;;;	*) Interactive function "narrow buffer to region"
;;;	*) Function to pop up a page, and then return directly to the
;;;	   previous position.  Maybe make general?  marks probably do that.
;;;	*) Completion in C code needs to be on C symbols, not partial symbols.
;;;	*) Play with completion.
;;;	*) Mouse command to bring line I click on to top of screen.
;;;	*) Deal somehow with file aliasing through symbolic links.

(setq load-path (cons "~rsmith/lib" load-path))

;;; Stuff from Linux provided .emacs.  This is picked and chosen; check
;;; out ~randy/.emacs.distrib on tigana for the whole thing.

;;; Vt100 Esc-[ keymap stuff.

(defvar cursor-map-2 (make-keymap)
"for ESC-[")
(fset 'Cursor-Map-2 cursor-map-2)
(define-key esc-map "[" 'Cursor-Map-2)

(define-key esc-map "[A" 'previous-line)
(define-key esc-map "[B" 'next-line)
(define-key esc-map "[C" 'forward-char)
(define-key esc-map "[D" 'backward-char)
(define-key esc-map "[H" 'beginning-of-line)
(define-key esc-map "[Y" 'end-of-line)
(define-key esc-map "[5^" 'scroll-down)
(define-key esc-map "[6^" 'scroll-up)
(define-key esc-map "[[A" 'help-for-help)
(define-key esc-map "[[B" 'byte-compile-file)
(define-key esc-map "[[C" 'isearch-forward)
(define-key esc-map "[[D" 'query-replace-regexp)
(define-key esc-map "[[E" 'eval-defun)
(define-key esc-map "[[F" 'eval-current-buffer)
(define-key esc-map "[[G" 'buffer-menu)
(define-key esc-map "[[H" 'global-set-key)
(define-key esc-map "[[I" 'save-buffer)
(define-key esc-map "[[J" 'save-buffers-kill-emacs)
(define-key esc-map "[2^" 'set-mark-command)
(define-key esc-map "[3^" 'delete-char)

;;; Auto load stuff.

(autoload 'ispell-word "ispell"
  "Check the spelling of word in buffer." t)
(autoload 'ispell-region "ispell"
  "Check the spelling of region." t)
(autoload 'ispell-buffer "ispell"
  "Check the spelling of buffer." t)
(autoload 'ispell-complete-word "ispell"
  "Look up current word in dictionary and try to complete it." t)
(autoload 'ispell-change-dictionary "ispell"
  "Change ispell dictionary." t)

;;; Check if something's on the load path.

(defun file-on-load-path (filename)
  (and load-path
       (or (file-exists-p (concat (car load-path) "/" filename))
	   (let ((load-path (cdr load-path)))
	     (file-on-load-path filename)))))


;;; Mail stuff.

(if (getenv "MAIL") (setq rmail-spool-directory
   (file-name-directory (getenv "MAIL"))))

;;; Back to Randy's homegrown stuff.

;;; I want to be able to override stuff in default.el, so load it first.
(if (load "default" t t)
  (setq inhibit-default-init t))

;;; Need to setup tags stuff XXX

;;; Rmail summary extensions?  Check if there's new stuff in 19 here.  XXX

;;; Rmail extensions for procmail support.

(defun randy-dired-rmail-input ()
  "Visit current file in dired as rmail file.
Pull in new mail if any."
  (interactive)
  (rmail-input (dired-get-filename))
  (rmail-get-new-mail))

;;; Variables to effect emacs' behavior to be more like I'd like.
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq find-file-visit-truename t)

(setq version-control t)

;;; (defun write-backup-file ()
;;;   "Force writing of a backup file (using numberic or regular backups as
;;; controlled by the setting of version-control)"
;;;   (interactive)
;;;   (write-region
;;;    (point-min)
;;;    (point-max)
;;;    (car (find-backup-file-name buffer-file-name))
;;;    nil nil))

;;; Get it to ask if I want a final newline, and setup to change this.
(setq require-final-newline 1)

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

;(setq c-continued-statement-offset 4)
;; c-indent-region is a pile of excrement and none can abide the odour
;; thereof.
;(add-hook 'c-mode-hook '(lambda ()
;			  (setq indent-region-function nil)
;			  (setq adaptive-fill-regexp
;				"[ 	]\\(\\*[ 	]*\\)?")))
;(add-hook 'c++-mode-hook '(lambda ()
;			  (setq adaptive-fill-regexp
;				"[ 	]\\(\\*[ 	]*\\|//[ 	]*\\)?")))

;;; Dired stuff not put in; I need to decide what to keep and what to
;;; toss.  Stuff that would be included:   (XXX)
;;;	*) print file on current line (includes dealing with dvi files).
;;;	*) Enscript file on current line
;;;	*) Execute current file.
;;;	*) Copy current file to <file>.distrib
;;;	*) Make a directory.
;;;	*) UUencode the current file.  UUdecode the current file.
;;;	*) Visit Makefile in directory under cursor.
;;;	*) Flag (for deletion) all files matching regexp.
;;;	*) Flag all files ending in .o (for deletion).
;;;	*) Read current file into rmail.
;;;	*) Load current file as emacs lisp code.

;;; Print current message in rmail to printer?.  XXX

;;; Shell-mode function to send a cd to the shell to move to
;;; whatever directory is current in the other window.  Does current
;;; dired have anything like this?  XXX

;;; Rmail function to re-mail a failed message.  Rmail function to resend a
;;; message returned because of "user unknown".  XXX

;;; In rmail summary mode, output message under cursuo to an rmail file.
;;; XXX

;;; In an rmail message, append the body of the message to a file.
;;; XXX

;;; Functions to force deletion of symlinks when they are saved to.
;;; XXX

;;; List all (C) functions within a file.
;;; XXX

;;; Surround the current line (that isn't preceded or followed by spaces)
;;; with equals signs.

(defun randy-current-line-boundaries ()
  "Returns a cons sell, the two elements of which are the first and
last non-whitespace characters on a line, respectively."
  (save-excursion
    (cons (progn (back-to-indentation) (current-column))
	  (progn (if (re-search-backward "[^ 	]"
					 (prog1 (point) (end-of-line)) 1)
		     (forward-char 1))
		 (current-column)))))

(defun randy-equals-bracket ()
  "Bracket the text on the current line with equals signs."
  (interactive)
  (let ((text-pos (randy-current-line-boundaries)))
    (beginning-of-line)
    (insert-char ?  (car text-pos))
    (insert-char ?= (- (cdr text-pos) (car text-pos)))
    (insert-char 10 1)
    (end-of-line)
    (insert-char 10 1)
    (insert-char ?  (car text-pos))
    (insert-char ?= (- (cdr text-pos) (car text-pos)))
    (insert-char 10 1)))

(defun randy-dash-underline ()
  "Underline the text on the current line with dashes."
  (interactive)
  (let ((text-pos (randy-current-line-boundaries)))
    (end-of-line)
    (insert-char 10 1)
    (insert-char ?  (car text-pos))
    (insert-char ?- (- (cdr text-pos) (car text-pos)))
    (insert-char 10 1)))

;;; Extra rmail functions

;;; Setup the visit menu bar item.
(defun randy-menu-visit-mail-file ()
  (interactive)
  (rmail-input (concat "~/Mail/" (symbol-name last-command-event)))
  (rmail-get-new-mail))

(defvar randy-mail-menu-map (make-sparse-keymap "Mail files"))
(fset 'randy-mail-menu-var randy-mail-menu-map)

(defun randy-setup-mail-menu ()
  (save-excursion
    (let (filename done)
      (set-buffer (find-file-noselect "~/Mail/.order"))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (while (not done)
	(setq filename
	      (buffer-substring (point) (progn (end-of-line) (point))))
	(define-key randy-mail-menu-map
	  (vector (intern filename))
	  (cons filename 'randy-menu-visit-mail-file))
	(if (not (eq (forward-line -1) 0))
	    (setq done t))))))

;(add-hook 'rmail-mode-hook
;	  '(lambda ()
;	     (randy-setup-mail-menu)
;	     (define-key rmail-mode-map [menu-bar visit]
;	       (cons "Visit" randy-mail-menu-map))))

;; Stuff involving saving rmail files if too long idle.

(defvar heartbeat-last-recent-keys nil
  "Last output from (recent-keys) done for the heartbeat function")
heartbeat-last-recent-keys

(defun randy-save-all-rmail-files ()
  (let ((current-buflist (buffer-list)))
    (save-excursion
      (while current-buflist
	(set-buffer (car current-buflist))
	(if (eq major-mode 'rmail-mode)
	    (progn
	      (if (buffer-modified-p)
		  (let ((delete-old-versions nil)) (save-buffer)))
	      ;; I think that the save may fail, though that would
	      ;; probably generate an error.  I'd like it to simply
	      ;; leave the buffer alone and I'd deal later.
	      (if (not (buffer-modified-p))
		  (kill-buffer (car current-buflist)))))
	(setq current-buflist (cdr current-buflist))))))

(defun heartbeat-rmail-save ()
  (let ((rk-output (recent-keys)))
    (if (equal rk-output heartbeat-last-recent-keys)
	(randy-save-all-rmail-files)
	(setq heartbeat-last-recent-keys rk-output))))

;(run-at-time "1 hour" "1 hour" 'heartbeat-rmail-save)

;; Eventually I should provide this functionality in rmail-summary-mode.
(defun randy-rmail-resend-personal ()
  (interactive)
  (let ((personal-address "randy@world.std.com"))
    (rmail-resend "randy@world.std.com")
    (rmail-delete-forward)
    (message "Message resent to %s" personal-address)))

(defun rmail-summary-by-senders (senders)
  "Display a summary of all messages with the given SENDERS.
SENDERS is a string of names separated by commas."
  (interactive "sSenders to summarize by: ")
  (rmail-new-summary
   (concat "senders " senders)
   (list 'rmail-summary-by-senders senders)
   'rmail-message-senders-p
   (mail-comma-list-regexp senders)))

(defun randy-rmail-message-unlabeled-p (msg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (rmail-msgbeg msg))
      (forward-char 3)
      (and (re-search-forward ",," nil t)
	   (char-equal ?\n (following-char))))))

(defun randy-rmail-summarize-unlabeled ()
  "Create a summary listing of all of the messages that do *not* have labels."
  (interactive)
  (rmail-new-summary "*Unlabeled*"
		     '(randy-rmail-summarize-unlabeled)
		     'randy-rmail-message-unlabeled-p))

(defun randy-countalist-increment (alist key)
  "Increment the count in ALIST associated with KEY.
This function treats ALIST as a associative list of (key . count) pairs,
and increment the count associated with KEY if KEY is in the alist.
If it is not, a new entry is crea(interactive)ted for KEY that has a count of 1.
The new ALIST is returned."
  (let ((ele (assoc key alist)))
    (if ele
	(setcdr ele (1+ (cdr ele)))
      (setq alist (cons (cons key 1) alist)))
    alist))

(defun randy-rmail-label-alist ()
  "Return an associative list of labels with number of occurrences."
  (let ((msgnum 1) label-alist
	label-string)
    (save-restriction
      (save-excursion
	(widen)
	(while (>= rmail-total-messages msgnum)
	  (goto-char (rmail-msgbeg msgnum))
	  (forward-char 3)
	  (re-search-forward ",," nil t)
	  (setq label-string
		(buffer-substring
		 (point) (save-excursion (end-of-line) (point))))
	  (while (string-match "^[ 	,]*\\([^ 	,][^ 	,]*\\)"
			       label-string)
	    (let ((this-label (substring label-string
					 (match-beginning 1)
					 (match-end 1))))
	      (setq label-alist
		    (randy-countalist-increment label-alist this-label))
	      (setq label-string (substring label-string (match-end 1)))))
	  (setq msgnum (1+ msgnum)))))
    label-alist))

(defun randy-rmail-label-list (update-labels)
  "Create a buffer with counts for each label used in the current rmail file."
  (interactive "P")
  (let (label-alist label-buffer label-list (buf-readonly buffer-read-only))
    (setq label-alist (sort (randy-rmail-label-alist)
			    '(lambda (first second)
			       (> (cdr first) (cdr second)))))
    ; If requested, update the buffer alist.
    (if update-labels
	(progn
	  (mapcar '(lambda (label)
		     (setq label-list (concat label-list
					      (and label-list ",")
					      (car label))))
		  label-alist)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (if buf-readonly (toggle-read-only))
	      (goto-char (point-min))
	      (re-search-forward "^Labels:")
	      (delete-region (point) (progn (end-of-line) (point)))
	      (insert label-list)
	      (if buf-readonly (toggle-read-only))))))
    (setq label-list (mapcar 'car label-alist))
    (setq label-buffer
	  (get-buffer-create (concat "*" (buffer-name) "-labellist*")))
    (save-excursion
      (set-buffer label-buffer)
      (erase-buffer)
      (while label-alist
	(insert (car (car label-alist)))
	(indent-to 40)
	(insert (number-to-string (cdr (car label-alist))) ?\n)
	(setq label-alist (cdr label-alist)))
      (goto-char (point-min)))
    (display-buffer label-buffer t)))

(defun randy-rmail-summary-by-labels (prefix labels)
  "Like rmail-summary-by-labels, except a prefix arg will force
an unlabelled summary."
  (interactive (list current-prefix-arg
		     (or current-prefix-arg
			 (read-from-minibuffer "Labels to summarize by: "))))
  (if (not prefix)
      (rmail-summary-by-labels labels)
    (randy-rmail-summarize-unlabeled)))

;;; If setup in mail-setup-hook, will include my personal headers
;;; (X-Pmd-Folder is the main one).
;;; (defun randy-rmail-include-headers ()
;;;   (if (and replybuffer
;;; 	   (save-excursion
;;; 	     (progn
;;; 	       (set-buffer replybuffer)
;;; 	       (eq major-mode 'rmail-mode))))
;;;       (save-excursion
;;; 	(goto-char (point-min))
;;; 	(re-search-forward "^--text follows this line--")
;;; 	(forward-line -1)
;;; 	(end-of-line)
;;; 	(insert "\nX-Pmd-Folder: " (buffer-name replybuffer)))))
;;;
;;; (add-hook 'mail-setup-hook 'randy-rmail-include-headers)

;;; Pull in some picture mode stu(interactive)ff (yank-rectangle-from-register).
(autoload 'picture-yank-rectangle-from-register "picture"
	  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
	  t)

;;; Function to replace current buffer with the result of that buffer
;;; run through "nroff -man" and pretified.
(defun randy-format-man-page ()
  (interactive)
  (let ((ro buffer-read-only))
    (save-excursion
      (if ro (toggle-read-only))
      (shell-command-on-region (point-min) (point-max) "nroff -man"  t)
      (goto-char (point-min))
      (replace-regexp "." "")
      (not-modified)
      (if ro (toggle-read-only)))))

(load-library "cc-mode")

(c-add-style "randy" '("gnu" (c-basic-offset . 4)
		       (c-offsets-alist . ((arglist-intro . ++)))))
(c-add-style "DAPL" '("randy"
		       (c-offsets-alist . ((arglist-intro . +)
					   (substatement-open . 0)))))
(setq c-default-style '((c-mode . "DAPL") (other . "gnu")))


;;; Functions that allow you to visit and manipulate emacs' best guess
;;; as to what the filename under the cursor is.
;;; Disabled; followed by a version I like better.

;;; I'm going to disallow trailing periods; this may or may not be a good
;;; idea.
;;;(defun randy-filename-under-point ()
;;;  (save-excursion
;;;    (let (m-begin m-end)
;;;      (re-search-backward "[^-a-zA-Z0-9/.#%@_~]" nil t)
;;;      (forward-char 1)
;;;      (setq m-begin (point))
;;;      (re-search-forward "[^-a-zA-Z/0-9.#%@_~]" nil t)
;;;      (forward-char -1)
;;;      ;; Disallow trailing period.
;;;      (if (equal (preceding-char) ?.)
;;;	  (forward-char -1))
;;;      (setq m-end (point))
;;;      (buffer-substring m-begin m-end))))
;;;
;;;(defun randy-find-file-under-point ()
;;;  "Run FIND-FILE on the filename under the current point."
;;;  (interactive)
;;;  (find-file (randy-filename-under-point)))
;;;
;;;(defun randy-find-file-other-window-under-point ()
;;;  "Run FIND-FILE-OTHER-WINDOW on the filename under the current point."
;;;  (interactive)
;;;  (find-file-other-window (randy-filename-under-point)))

(defun randy-word-under-point (&optional syntab wordsep)
  "Return a string which is the word underneath point in the current buffer.
Optionally specify an alternate syntax table in SYNTAB to define words.
Optionally specify a word separator WORDSEP.  If specify, the function
may return either a single word or a list of two words (the second
following the word point is on separated from it by WORDSEP)."
  (let* ((buffer-syntax-table (syntax-table))
	(local-syntax-table (or syntab buffer-syntax-table))
	word1 wordaft)
    (save-excursion
      (set-syntax-table local-syntax-table)
      (setq word1 (buffer-substring (progn (forward-word -1) (point))
				    (progn (forward-word 1) (point))))
      (if (equal (char-after (point)) wordsep)
	  (progn
	    (forward-char 1)
	    (setq wordaft (buffer-substring (point)
					    (progn (forward-word 1)
						   (point))))))
      (set-syntax-table buffer-syntax-table)
      (if wordaft
	  (list word1 wordaft)
	word1))))

(defconst randy-find-file-under-point-syntab
  (let ((syntab (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" syntab)
    (modify-syntax-entry ?- "w" syntab)
    (modify-syntax-entry ?+ "w" syntab)
    (modify-syntax-entry ?_ "w" syntab)
    (modify-syntax-entry ?/ "w" syntab)
    (modify-syntax-entry ?~ "w" syntab)
    syntab)
  "Syntax table to use for distinguishing filenames in text.")

(defun randy-find-file-under-point (other-window-p)
  "Find file under point.  With a prefix arg, find it in the other window."
  (interactive "P")
  (let ((filename (randy-word-under-point randy-find-file-under-point-syntab ?:))
	wordafter)
    (if (consp filename)
	(progn
	  (setq wordafter (car (cdr filename)))
	  (setq filename (car filename))))
    (if (string-match "^.*\\.$" filename)
	(setq filename (substring filename 0 -1)))
    (if other-window-p
	(find-file-other-window filename)
      (find-file filename))
    (if (and wordafter (string-match "^[0-9]*$" wordafter))
	(goto-line (string-to-number wordafter)))
    (if other-window-p
	(other-window 1))
    ))

(defun randy-find-burt (filename &optional other-window-p)
  "Find burt.  Default is under point.
With a prefix arg, find it in the other window."
  (interactive (list
		(let ((defburt (randy-word-under-point)))
		  (read-string (concat "Burt to edit (" defburt "): ")
			       nil nil
			       defburt))
		current-prefix-arg))
  (let (wordafter)
    (if (not (string-match "^[0-9]+$" filename))
	(error (concat "Attempted to find non-numeric BURT: " filename)))
    (setq filename (concat "~/.burt/" filename))
    (if other-window-p
	(find-file-other-window filename)
      (find-file filename))
    (if other-window-p
	(other-window 1))
    ))

;;; Function to send a ^C to a process without following it with a
;;; newline.
(defun send-unbuffered-break ()
  (interactive)
  (let ((process (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process"))))
    (process-send-string process "")))

(defun randy-send-unbuffered-stri(interactive)ng (string)
  "Send a arbitrary string to process with no newline at the end."
  (interactive "sString to send: ")
  (let ((process (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process"))))
    (process-send-string process string)))

;;; Function for shifting fonts in X.  I'm going to delay this, since
;;; I don't use it really, and I'm sure the X interface has changed.
;;; XXX.

;;; Functions to put me in editing my todo files. XXX

;;; Map bindings

;;; Redefinitions of global keys.
(define-key esc-map ":" 'goto-line)
(define-key esc-map "?" 'what-line)
(define-key esc-map "+" 'pop-tag-mark)
;;;(define-key global-map "\C-o" 'randy-just-open-line)

;;; Mode specific map bindings
;;; I use a, c, f, g, h, m, r, t, w
;;; I think l is neat; I don't think I've been using it.
;;; I don't remember p, q (XXX--more mnemonic bindings?)
;;; Unused: e, i-k, n, s, u, x-y

;;; I've commented out those definitions for which I don't yet have
;;; the "backing functions" setup.  I've deleted a few I don't
;;; think I'm using or going to use.

;(define-key mode-specific-map "a" 'pages-directory-for-addresses)
;;; "b" is a prefix key for TAGS menu.  See below.
(define-key mode-specific-map "c" 'compile)
;;; "d" is used in shell mode.
(define-key mode-specific-map "e" '(lambda () (interactive)
				     (shell-command "date" t)))
(define-key mode-specific-map "f" 'randy-find-file-under-point)
;(define-key mode-specific-map "4f" 'randy-find-file-other-window-under-point)
;(define-key mode-specific-map "4\C-f"
;  'randy-find-file-other-window-under-point)
(define-key mode-specific-map "g" 'grep)
(define-key mode-specific-map "h" 'shell)
;;; "i" is used for glimpse, and "j" for rgrep.  Should probably be here.
;;; "k" is used as prefix for console connections in netapp
(define-key mode-specific-map "l" '(lambda () (interactive)
				    (setq truncate-lines
				     (not truncate-lines))))
(define-key mode-specific-map "m" 'randy-format-man-page)
(define-key mode-specific-map "n" 'randy-find-burt)
(define-key mode-specific-map "o" 'overwrite-mode)
(define-key mode-specific-map "p" 'copy-rectangle-to-register)
(define-key mode-specific-map "q" 'picture-yank-rectangle-from-register)
(define-key mode-specific-map "r"
  '(lambda ()
     (interactive)
     (revert-buffer t t)))
(define-key mode-specific-map "t" 'rmail-input)
(define-key mode-specific-map "u" 'randy-uniquename-buffer)
(define-key mode-specific-map "v" 'set-vi-type-indent)
(define-key mode-specific-map "w" 'write-region)
;;; "x" is used in rmail mode for resend.
;;; "z" is used in outline mode for inserting ">> Prioritize^J^J<<"

(define-key mode-specific-map ">" '(lambda () (interactive)
				    (randy-shift-font 1)))
(define-key mode-specific-map "<" '(lambda () (interactive)
				    (randy-shift-font -1)))
(define-key mode-specific-map "^" 'randy-set-window-height)

;;; And put in the damn FKEY defs that you use all the time anyway.
(define-key global-map '[f12] 'quoted-tab)
(define-key global-map '[f9] 'next-error)

;;; "z" used in c mode.

;;; Tags stuff.  Nothing setup here yet, but there will be.
;;; XXX best thing to do here is to have some keystroke pop up a menu
;;; (what happens when not under X) of sorted list of all tags tables
;;; registered.

(defun randy-visit-tags-table (table)
  "Visit a tags table, using old semantics."
  (interactive)
  (setq tags-file-name table)
  (setq tags-table-list nil))

(define-key mode-specific-map "bk"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/smp_shared/src/mach_kernel/TAGS")))

(define-key mode-specific-map "bK"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/self_host/src/mach_kernel/TAGS")))
(define-key mode-specific-map "bS"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/self_host/src/osf1_server/TAGS")))


(define-key mode-specific-map "be"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/nmk19_maint/src/osf1_server/TAGS")))

(define-key mode-specific-map "b1"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/nmk19_maint/src/osf1ad/TAGS")))

(define-key mode-specific-map "bl"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/lib/TAGS.emacs-19.34")))

(define-key mode-specific-map "bf"
  '(lambda () (interactive)
     (randy-visit-tags-table "~/sb/nmk19_maint/src/mach_services/lib/libflipc/TAGS.all")))

(define-key mode-specific-map "b "
  '(lambda () (interactive)
     (setq tags-file-name nil)
     (setq tags-table-list nil)
     (message "TAGS state nulled.")))

;;; Mode specific map defines.
(define-key mode-specific-map "%" 'query-replace-regexp)
(define-key mode-specific-map "\C-?" 'backward-kill-sexp)
(define-key mode-specific-map "\C-k" 'kill-sexp)
(define-key mode-specific-map "." 'find-yacc-tag)

;;; Mode hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq mail-mode-hook
      '(lambda ()
	(auto-fill-mode 1)
	(setq fill-prefix "  ")
	(setq mail-yank-prefix "> ")
	(define-key mail-mode-map "\C-c\C-r" 'randy-include-signature)
	(define-key mail-mode-map "\C-\M-q"
	  'randy-mail-inserted-message-fill-paragraph)))

(add-hook 'rmail-mode-hook
	  '(lambda ()
	     (setq rmail-delete-after-output t)
	     (define-key rmail-mode-map "l" 'randy-rmail-summary-by-labels)
	     (define-key rmail-mode-map "L" 'randy-rmail-label-list)
	     (define-key rmail-mode-map "\C-cx" 'randy-rmail-resend-personal)))
(defun randy-include-signature ()
  (interactive)
  (insert "

						-- Randy

"))

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

;;; Cd function would be good XXX.

(defun randy-shell-cd-directory-other-window (pushd-p)
  "Cd to the current directory of the other window on the screen.
With prefix arg, does a pushd rather than a cd.
Only valid in a shell window."
  (interactive "P")
  (let (cddir (cmd (if pushd-p "pushd" "cd")))
    (other-window -1)
    (setq cddir default-directory)
    (other-window 1)
    (goto-char (point-max))
    (insert cmd " " cddir)
    (comint-send-input)))

;;; Define a replacement for an internal comint function to allow us to
;;; fake out bash about whether it's really being run from an emacs or not.
;;; The real solution to this problem is to fix bash.

(defun randy-comint-exec-1 (name buffer command switches)
  (let ((process-environment(interactive)
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `unknown' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "EMACS=t" "TERM=unknown"
		    (format "COLUMNS=%d" (frame-width)))
	    (list "EMACS=t" "TERM=emacsr"
		  (format "TERMCAP=emacsr:co#%d:tc=unknown" (frame-width))))
	  process-environment)))
    (apply 'start-process name buffer command switches)))

(defun randy-add-my-comint-exec-1 ()
  (fset 'comint-exec-1 (symbol-function 'randy-comint-exec-1)))

(add-hook 'comint-mode-hook 'randy-add-my-comint-exec-1)

(defun randy-shell-mode-hook-function ()
    (define-key shell-mode-map "\C-c\C-c" 'send-unbuffered-break)
    (define-key shell-mode-map "\C-c\C-q" 'randy-send-unbuffered-string)
    (define-key shell-mode-map "\C-cd" 'randy-shell-cd-directory-other-window)
)

(defun randy-gdb-mode-hook-function ()
    (define-key (current-local-map) "\C-c\C-q" 'randy-send-unbuffered-string))

(add-hook 'shell-mode-hook 'randy-shell-mode-hook-function)
(add-hook 'gdb-mode-hook 'randy-gdb-mode-hook-function)

;;; Leaving out dired stuff to readin directories; used for easy playing
;;; with dired-listing switches.  I believe that there's builtins for
;;; this now.   XXX
(defun randy-dired-compress-print (flags-prompt)
  "Print a file double up, on both sides of the page."
  (interactive "P")
  (shell-command
   (concat "psprint -2 -s7 "
	   (dired-get-filename)
	   "| psduplex | lpr "
	   (if flags-prompt (read-from-minibuffer "Flags to lpr: ") ""))))

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map "w" 'randy-dired-compress-print)
	     (define-key dired-mode-map "r"
	       'randy-dired-create-matching-rmail-file)
	     (define-key dired-mode-map "I"
	       'randy-dired-rmail-input)))

;;; Dired bindings left out. XXX

;;; Defaults I'd like to change.
(setq-default case-fold-search nil)

;;; Skipped rmail prefix stuff.  XXX

;;; Skipped kill current diff. XXX

;;; Other hooks.

;;; find file hook to let me look at compressed files without uncompressing
;;; them explicitly.  Eventually I'll probably need to program in a way
;;; around this.

(defun randy-check-file-visiting-compressed ()
  (let ((command
	 (if (string-match ".*\\.gz$" (buffer-file-name))
	     "gunzip"
	   (if (string-match ".*\\.Z$" (buffer-file-name))
	       "zcat" nil))))
    (if (not command)
	nil
      (setq buffer-read-only nil)
      (shell-command-on-region (point-min) (point-max) "gunzip" t)
      (goto-char (point-min))
      (not-modified)
      (setq buffer-read-only t))))

(add-hook 'find-file-hooks 'randy-check-file-visiting-compressed)

;;; (defvar randy-tags-location-stack nil
;;;   "List of locations that we've executed a find-tag from.
;;; Does not include find-tag's done to get symbol definitions after the
;;; first, nor find-tag's used to go to previous tag positions.")
;;;
;;; ;;; Relies on 'nextp' being defined as it is in find-tag-noselect,
;;; ;;; from which this is called.
;;; (defun randy-find-tag-hook-function ()
;;;   (if (eq next-p nil)                   ; Means this is a true find-tag
;;;       (setq randy-tags-location-stack
;;;             (cons (point-marker) randy-tags-location-stack))))
;;;
;;; (add-hook 'find-tag-hook-pre 'randy-find-tag-hook-function)
;;;
;;; (defun randy-pop-tag ()
;;;   (interactive)
;;;   (if randy-tags-location-stack
;;;       (let ((marker (car randy-tags-location-stack)))
;;;         (setq randy-tags-location-stack
;;;               (cdr randy-tags-location-stack))
;;;         (switch-to-buffer (marker-buffer marker))
;;;         (goto-char (marker-position marker))
;;;         (set-marker marker nil nil))
;;;     (message "No previous positions in location stack")))
;;;
;;; (setq load-path (cons "~rsmith/lib" load-path))
;;; (load "etags")
;;;
;;; (define-key esc-map "+" 'randy-pop-tag)

(setq gnus-default-nntp-server "news.netapp.com")

(setq sendmail-program "/usr/sbin/sendmail")

(setq rmail-delete-after-output t)

(load "rmail")
(load "rmail-inbox-hack")		; Overlay rmail-insert-inbox-text
(defun massage-windows-text ()
  (save-excursion (replace-string "" ""))
  (save-excursion (replace-string "—" "--"))
  ;; Need to hack double and single quotes.
  (save-excursion (replace-string "
" "

"))
  (setq fill-prefix nil)
  (save-excursion (fill-region (point) (point-max))))

;;; (setq user-mail-address "randy@world.std.com")

;;; Temporary until I get the emacs lisp startup setup the way I want.
(compile-if-newer-then-load "/u/rsmith/Config/Tools/mh/mh")

(fset 'quoted-tab [?\C-q tab])

;;; Orca specific (gotta get this split out into my directories)

(compile-if-newer-then-load "/u/rsmith/lib/grep-expanded")

;;; History variables for the different forms of "grep" I'm likely to use.
(defvar glimpse-history nil "History variable for glimpse runs.")
(defvar egrep-history nil "History variable for egrep runs.")
(defvar fsfile-history nil "History variable for fsfile runs.")
(defvar rgrep-history nil "History variable for rgrep runs.")

(defvar randy-sandbox-directory "/u/rsmith/Sandbox/"
  "Top of Sandbox tree currently being worked on.
All commands that reference this should be sensitive to in-session
changes to it.
Should always have a trailing '/' (emacs canonical directory)")

(defconst randy-sandboxen-directory "/u/rsmith/Sandboxen/"
  "Directory into which sandboxes are placed.")

(defun randy-change-default-sandbox (sandboxdir &optional dontstartshell)
  (interactive "DNew sandbox directory: ")
  (setq randy-sandbox-directory
	(file-name-as-directory (expand-file-name sandboxdir)))
  ;;; Null out histories; they don't really apply between sandboxes.
  (setq glimpse-history nil)
  (setq egrep-history nil)
  (setq fsfile-history nil)
  ;;
  (let ((tags-add-tables nil))
    (visit-tags-table
     (concat randy-sandbox-directory "/TAGS")))
  (string-match "\\(\\(.*\\)/\\)*\\([^/][^/]*\\)/?$" sandboxdir)
  (randy-set-frame-name (substring sandboxdir
				   (match-beginning 3)
				   (match-end 3)))
  ;; For running perldb on ntest
  (setenv "SAWZALL_DIR" (expand-file-name sandboxdir))
  (if (not dontstartshell)
      (progn
	(find-file sandboxdir)
	(delete-other-windows)
	(split-window-vertically)
	(other-window 1)
	(shell))))

(defconst glimpse-base-command "glimpse -n -y -H"
  "Default command to use for running glimpse.")
(defun randy-run-glimpse ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command
     (concat glimpse-base-command randy-sandbox-directory)
     t 'glimpse-history)))

(defconst rgrep-command-format-string "rgrep"
  "String to use as base for recursive egrep runs.")

(defun randy-run-egrep-recursively ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command rgrep-command-format-string t 'egrep-history)))

(defconst fsfile-command-string "fsfile -a -s "
  "String to use as a base for finding file names.")

(defun randy-run-fsfile ()
  (interactive)
  (let ((grep-null-device "") (null-device ""))
    (grep-like-command (concat fsfile-command-string randy-sandbox-directory)
		       t 'fsfile-history)))

(define-key mode-specific-map "i" 'randy-run-glimpse)
(define-key mode-specific-map "j" 'randy-run-egrep-recursively)
(define-key mode-specific-map "z" 'randy-run-fsfile)
(define-key mode-specific-map "s" 'randy-change-default-sandbox)

(compile-if-newer-then-load "split")

(add-hook 'outline-mode-hook 'hide-body)
(add-hook 'outline-mode-hook
	  '(lambda ()
	     (define-key outline-mode-map "\C-cz"
	       '(lambda ()
			(interactive)
			(beginning-of-line)
			(insert ">> Prioritize\n\n\n<<\n\n")
			(forward-line -3)))))

(defun randy-highlight-word (name)
  "Hilight all instances of NAME in the current buffer.
With a prefix argument, highlight them even as part of a larger word."
  (interactive "sIdentifier to highlight: ")
  (let ((highlight-regexp (concat "\\b" (regexp-quote name) "\\b"))
	(mod (buffer-modified-p))
	(ro buffer-read-only))
    (save-excursion
      (if ro (toggle-read-only))
      (goto-char (point-min))
      (while (re-search-forward highlight-regexp (point-max) t)
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(face highlight)))
      (set-buffer-modified-p mod)
      (if ro (toggle-read-only)))))

(defun randy-remove-highlighting ()
  "Remove all highlighting within the buffer."
  (interactive)
  (let ((mod (buffer-modified-p))
	(ro buffer-read-only))
    (if ro (toggle-read-only))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (set-buffer-modified-p mod)
    (if ro (toggle-read-only))))

(defvar explode nil "List of headings to explode in outline mode.")

(defun randy-explode-hook ()
  "Explode items mentioned in EXPLODE variable."
  (setq randy-save-explode explode)
  (setq randy-save-mm major-mode)
  (if (and explode (eq major-mode 'outline-mode))
      (save-excursion
	(let ((explode-re (concat "^\\*\\** " explode)))
	  (goto-char (point-min))
	  (while (re-search-forward explode-re (point-max) t)
	    (show-children)
	    (show-entry))))))

(add-hook 'hack-local-variables-hook 'randy-explode-hook t)

(defun randy-open-console-connection (machine)
  (interactive "sMachine to connect to: ")
  (let* ((entry (assoc machine console-list))
	 (termserv (and entry (nth 1 entry)))
	 (port (and entry (nth 2 entry)))
	 (buffer-name (concat "*" machine "-console*"))
	 (existing-buffer (get-buffer buffer-name)))
    (if (not entry)
	(error (concat "Machine "(interactive) machine " has unknown console."))
      (if (and existing-buffer
	       (comint-check-proc existing-buffer))
	  (switch-to-buffer existing-buffer)
	(if existing-buffer
	    (progn
	      (set-buffer existing-buffer)
	      (rename-uniquely)))
	(telnet (concat termserv " " (int-to-string port)))
	(set-buffer (concat "*telnet-" termserv " " (int-to-string port) "*"))
	(rename-buffer (concat "*" machine "-console*"))))))

;; As one body so that C-M-x will get the whole thing.
(progn
  (defconst console-list
    ;; Machine		Term server	port	C-ck <key>
    '(("heron"		"hydra"		9036	" ")
      ("scow"		"medusa"	9027   	" ")
      ("dodgy"		"hydra"		9001 	" ")
      ("shrimper"	"hydra"		9033 	" ")
      ("jester"		"hydra"		9021 	" ")
      ("snapper"	"cyclops"	9044 	"s")
      ("snipe"		"medusa"	9056	" ")
      ("squib"		"medusa" 	9025 	" ")
      ("orca-sun1"	"hydra"		9022 	" ")
      ("orca-sun2"	"hydra"		9025 	" ")
      ("elastica"	"hydra"		9013 	" ")
      ("gin"		"hydra"		9040 	" ")
      ("tonic"		"hydra"		9041	" ")
      ("orca-sun3"	"hydra"		9049 	" ")
      ("kipper"		"medusa"	9051 	" ")
      ("orca-sun6"	"medusa"	9052 	" ")
      ("pacer"		"medusa"	9032	" ")
      ("etchells"	"medusa"	9011	" ")
      ("orca-sun4"	"medusa"	9005	" ")
      ("sonar"		"medusa"	9009	" ")
      ("trapper"	"hydra"		9005	" ")
      ("spinel"		"cyclops"	9012	"p")
      ("marlin"		"hydra"		9034	" ")
      ("redwing"	"hydra"		9024	" ")
      ("kestrel"	"gorgon"	9002	" ")
      ("mansun"		"medusa"	9058	" ")
      ("soling" 	"hydra"		9029	" ")
      ("bones"		"hydra"		9052	" ")
      ("trapper"	"hydra"		9005	" ")
      ("breeder"	"hydra" 	9003	" ")
      ("moray"		"hydra"		9013	" ")
      ("hornet"		"hydra"		9024	" ")
      ("nacra"		"hydra"		9023	" ")
      ("wayfarer"	"hydra" 	9044	" ")
      ("sailfish"	"medusa"	9006	" ")
      ("stilton"	"medusa"	9050	" ")
      ("port"		"medusa"	9049	" ")
      ("phantom"	"hydra"		9050	" ")
      ("orca-sun8"	"gorgon"	9031	" ")
      ("trouble"	"medusa"	9047	" ")
      ("strife"		"medusa"	9048	" ")
      ("dat-linux1"	"gorgon"	9004	"l")
      ("dat-linux2"	"gorgon"	9005	"2")
      ("dat-linux3"	"gorgon"	9006	" ")
      ("dat-linux4"	"gorgon"	9007	" ")
      ("dat-linux5"	"gorgon"	9008	" ")
      ("dat-linux6"	"gorgon"	9009	" ")
      ("dat-linux7"	"gorgon"	9027	" ")
      ("dat-linux8"	"gorgon"	9028	" ")
      ("dat-linux9"	"gorgon"	9048	" ")
      ("bond"		"poseidon"	9001	" ")
      ("x-parrot"	"medusa"	9003	"x")
      ;;; Diag port for x-parrot is medusa 9060
      ("x-men"		"medusa"	9002	" ")
      ("pons"		"gorgon"	9027	" ")
      ("fleischmann"	"gorgon"	9028	" ")

      ("childs"		"poseidon" 	9004	" ")
      ("bork"		"gorgon"	9048	" ")
      ("exorcist"	"poseidon"	9014	"t")
      ("id"		"poseidon"	9038	" ")
      ("ego"		"poseidon"	9039	" ")
      ("fear"		"rtpras2k5.hq.netapp.com"  6014		" ")
      ;; Want to put terror in but can't yet.
      ("bonham"		"medusa"	9041	" ")
      ("lark"		"medusa"	9019	" ")
      ("sprat" 		"poseidon"	9022	" ")
      ("sigma"		"cyclops" 	9016	" ")
      ("albacore"	"medusa"	9021	"a")
      ("nane-hp6"	"cerberus"	9033	"6")
      ("nane-hp9"	"cyclops"	9028	"9")
      ("nqa-hp2"	"gorgon"	9050	" ")
      ("bosun"		"medusa"	9018	"b")
      ("nane-aix6" 	"cyclops"	9027	" ")
      ("nane-hp12"	"argus"		9001	"h")
      ("chips"		"cerberus" 	9012	" ")
      ("nane-aix9"	"argus"		9003	"i")
      ("nane-hp7"	"cerberus"	9034	"7")
      ("nane-hp12"	"argus"		9001	"c")

;;;      ("hurricane"	)
;;;      ("melges"		)
      )
    "List of machine consoles for randy-open-console-connection")

  ;; Do key bindings requested by console-list.
  (let ((active-list console-list))
    (while active-list
      (let* ((entry (car active-list))
	     (key (nth 3 entry))
	     (machine (car entry)))
	(if (and key (not (equal key " ")))
	    (define-key mode-specific-map (concat "k" key)
	      `(lambda () (interactive)
		 (randy-open-console-connection ,machine))))
	(setq active-list (cdr active-list)))))
  )

(defun randy-telnet-quit ()
  (interactive)
  (randy-send-unbuffered-string "")
  (randy-send-unbuffered-string "quit
"))

(defun randy-telnet-send-break ()
  (interactive)
  (randy-send-unbuffered-string "")
  (randy-send-unbuffered-string "send break
"))

;;; Put above in key bindings.


(add-hook 'comint-mode-hook
	  '(lambda ()
	     (define-key mode-specific-map "k\C-a" 'randy-telnet-send-break)
	     (define-key mode-specific-map "k\C-c" 'randy-telnet-quit)))

(setq compilation-scroll-output t)

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

(defun target-sandbox (sb-name &optional purpose)
  "Setup the current emacs to target sandbox SB-NAME with purpose PURPOSE."
  (let ((fname (concat (upcase sb-name))))
    (if purpose
	(setq fname (concat fname " -- " purpose)))
    (randy-change-default-sandbox (concat randy-sandboxen-directory sb-name))))

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

(fset 'randy-box
   [?\M-x ?r ?a ?n ?d ?y ?  ?d ?a ?  ?  return ?\C-p ?\C-k ?\C-y ?\C-p ?\C-p ?\C-a ?\C-y ?\C-c ?> ?- ?+ backspace ?\C-c ?. ?+ ?| ?\C-c ?< ?+ ?- ?\C-a ?\C-p ?\C-p ?\C-  ?\C-n ?\C-n ?\C-n ?\C-f ?\C-f ?\C-x ?r ?o ?\C-f ?\C-c ?< ?- ?\C-c ?. ?+ ?| ?\C-c ?> ?+ ?-])

(setq tags-revert-without-query t)
(setq delete-old-versions t) ; Almost never use backup; standard fine.

(defun function-occur ()
  "List the functions defined in a C file."
  (interactive)
  (occur "^[a-zA-Z_].*(\\($\\|.*[^;
]$\\)" nil))

(add-hook 'comint-mode-hook
	  '(lambda ()
	     (define-key comint-mode-map "\C-i" 'comint-dynamic-complete)))

(defun randy-popup-home-frame ()
  (interactive)
  (save-excursion
    (find-file "/u/rsmith/Config/HomeDisplay")
    (let ((home-display (buffer-substring (point-min) (point-max))))
      (make-frame-on-display home-display
			     '((left . -1) (top . 0)
			       (width . 80) (height . 50)
			       (user-position . t) (font . "7x14"))))))

(defun randy-popup-work-frame ()
  (interactive)
  (save-excursion
    (let ((work-display "rsmith-linux.nane.netapp.com:0.0"))
      (make-frame-on-display work-display
			     '((left . -1) (top . 0)
			       (width . 80) (height . 50)
			       (user-position . t) (font . "7x14"))))))

(add-hook 'signal-USR1-hook 'randy-popup-home-frame)

(defun randy-setup-sandbox-emacs (sandbox-name &optional rows)
  "Setup emacs as you prefer for sandboxes.  
Assumes initial frame is in upper right corner.  Columns are 80;
ROWS defaults to 50 unless specified.
Left is temporarily set at -745 as I can't find full informaton on
existing frames."
  (let ((frame-name (concat "Emacs: " sandbox-name))
	(existing-frame-left
	 (cdr (assoc 'left
		     (car (cdr (car (cdr (current-frame-configuration)))))))))
    (if (not rows) (setq rows 50))
    (make-frame
     (append
      '((top . 0) (width . 80) (left . -745))
      (list (cons 'height rows))))
    (target-sandbox sandbox-name)))

;;; Random DAPL code

;;(defvar randy-gdb-default-sandbox "/u/rsmith/Sandboxen/dapl1_sf")
;;(defvar randy-gdb-default-sandbox "/u/rsmith/Sandboxen/daplrch_sf")
;;(defvar randy-gdb-default-sandbox "/u/rsmith/Sandboxen/tr_dapl")
(defvar randy-gdb-default-sandbox (getenv "DAPL_TEST_SANDBOX"))

(defun randy-startup-gdb (program)
  (interactive "fProgram to debug: ")
  (setq program (concat "gdb " program))
  (let ((pname (file-name-nondirectory program)))
    (gdb program)
    (set-buffer (concat "*gud-" pname "*"))))

(defun randy-startup-gdb-winput (program input)
  (randy-startup-gdb program)
  (point-max)
  (insert input)
  (comint-send-input))

(defun randy-startup-gdb-dapltestclient ()
  (interactive)
  (randy-startup-gdb-winput
   (concat randy-gdb-default-sandbox "/test/udapl/dapltest/dapltest")
   "source /u/rsmith/Projects/DAPL/dapltestargs/.gdbinit_client"
   )
  (randy-change-default-sandbox randy-gdb-default-sandbox t)
  (randy-set-frame-name "dapltest client"))

(defun randy-startup-gdb-dapltestserver ()
  (interactive)
  (randy-startup-gdb-winput
   (concat randy-gdb-default-sandbox "/test/udapl/dapltest/dapltest")
   "source /u/rsmith/Projects/DAPL/dapltestargs/.gdbinit_server"
   )
  (randy-change-default-sandbox randy-gdb-default-sandbox t)
  (randy-set-frame-name "dapltest server"))


(fset 'goto-next-diff
   [?\C-e ?\C-\M-s ?^ ?\\ ?* ?\\ ?* ?\\ ?* ?\\ ?* ?\C-a escape ?0 ?\C-l])

(setq c-basic-offset 4)

(defun randy-wiki-prep ()
  "Fix a buffer using simplified wiki markup.
Changes a buffer using an altered wiki markup language in which
list item/indentation tags remain in effect until the next
paragraph break into one that uses the documented wiki markup.  This is
done by converting paragraphs that should all be using the above
wiki markup into very long lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\([*#:;\'].*\\)\\(\n[ 	]+[^ 	\n].*\\|\n[^ 	\n*#:;\'].*\\)+\n"
	    (point-max) t)
      (let ((beg (match-beginning 0)) (end (1- (match-end 0))))
	(goto-char beg)
	(while (search-forward "\n" end t)
	  (replace-match " "))))))


;;; Use ssh, not rsh
(setq remote-shell-program "ssh")

(defun randy-make-box (text-start text-end)
  "Create a box around text starting at TEXT-START and ending at TEXT-END.
This is 'ascii art' and assumes that the text is on a single line, there is
room around the text to create a box, and that the current buffer is in
picture mode.  Long term, I should put these check into the program."
  (interactive "d\nm\n")
  (save-excursion
    (let ((beg (make-marker))
	  (end (make-marker))
	  (diff (abs (- text-end text-start)))
	  i)
      (set-marker beg (min text-start text-end))
      (set-marker end (max text-start text-end))

      (picture-movement-right)
      (goto-char beg)
      (picture-backward-column 1)
      (picture-insert ?| 1)
      (picture-move-up 1)
      (picture-backward-column 1)
      (picture-insert ?+ 1)
      (setq i diff)
      (while (> i 0)
	(picture-insert ?- 1)
	(setq i (- i 1)))
      (picture-insert ?+ 1)
      (goto-char beg)
      ; (picture-backward-column 1)
      (picture-move-down 1)
      (picture-insert ?+ 1)
      (setq i diff)
      (while (> i 0)
	(picture-insert ?- 1)
	(setq i (- i 1)))
      (picture-insert ?+ 1)
      (picture-backward-column 1)
      (picture-move-up 1)
      (picture-insert ?| 1))))

;;; See if you can deal with emacs bindings for mac scroll wheel mouse
(progn
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 2)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 2)))
  (global-set-key [double-mouse-4] '(lambda () (interactive) (scroll-down 5)))
  (global-set-key [double-mouse-5] '(lambda () (interactive) (scroll-up 5)))
  (global-set-key [triple-mouse-4] '(lambda () (interactive) (scroll-down 8)))
  (global-set-key [triple-mouse-5] '(lambda () (interactive) (scroll-up 8))))

