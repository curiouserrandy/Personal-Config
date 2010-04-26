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


;;; Back to Randy's homegrown stuff.

;;; I want to be able to override stuff in default.el, so load it first.
(if (load "default" t t)
  (setq inhibit-default-init t))

;;; Need to setup tags stuff XXX

;;; (defun write-backup-file ()
;;;   "Force writing of a backup file (using numberic or regular backups as
;;; controlled by the setting of version-control)"
;;;   (interactive)
;;;   (write-region
;;;    (point-min)
;;;    (point-max)
;;;    (car (find-backup-file-name buffer-file-name))
;;;    nil nil))

(defun randy-countalist-increment (alist key)
  "Increment the count in ALIST associated with KEY.
This function treats ALIST as a associative list of (key . count) pairs,
and increment the count associated with KEY if KEY is in the alist.
If it is not, a new entry is created for KEY that has a count of 1.
The new ALIST is returned."
  (let ((ele (assoc key alist)))
    (if ele
	(setcdr ele (1+ (cdr ele)))
      (setq alist (cons (cons key 1) alist)))
    alist))

(load-library "cc-mode")

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

;;; Map bindings

;;; Redefinitions of global keys.
;;;(define-key global-map "\C-o" 'randy-just-open-line)

;;; Mode specific map bindings
;;; I use a, c, f, g, h, m, r, t, w
;;; I think l is neat; I don't think I've been using it.
;;; I don't remember p, q (XXX--more mnemonic bindings?)
;;; Unused: e, i-k, n, s, u, x-y

;;; I've commented out those definitions for which I don't yet have
;;; the "backing functions" setup.  I've deleted a few I don't
;;; think I'm using or going to use.


;;; Tags stuff.  Nothing setup here yet, but there will be.
;;; XXX best thing to do here is to have some keystroke pop up a menu
;;; (what happens when not under X) of sorted list of all tags tables
;;; registered.

(defun randy-visit-tags-table (table)
  "Visit a tags table, using old semantics."
  (interactive)
  (setq tags-file-name table)
  (setq tags-table-list nil))

;;; Mode specific map defines.

(add-hook 'rmail-mode-hook
	  '(lambda ()
	     (setq rmail-delete-after-output t)
	     (define-key rmail-mode-map "l" 'randy-rmail-summary-by-labels)
	     (define-key rmail-mode-map "L" 'randy-rmail-label-list)
	     (define-key rmail-mode-map "\C-cx" 'randy-rmail-resend-personal)))

;;; Cd function would be good XXX.

;;; Define a replacement for an internal comint function to allow us to
;;; fake out bash about whether it's really being run from an emacs or not.
;;; The real solution to this problem is to fix bash.

(defun randy-comint-exec-1 (name buffer command switches)
  (let ((process-environment
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

(compile-if-newer-then-load "split")

;;; Put above in key bindings.

;;; ------------------------------

