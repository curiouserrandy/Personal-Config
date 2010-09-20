;;; Functions for manipulating outline files used as todo files.

;;; See notes in Todomode.txt for design and format spec.

(require 'rs-outline)

(load-library "outline")		;Needed for outline-regexp

;;; ------------  Infrastructure implementation ----------------

;; This starts todo items.  They end at this regexp (next todo item),
;; outline-regexp, or the end of the file.
(defconst rstodo-todo-type-list '("--" "==" "??")
  "List of the types that a todo item may have (should do, can do, and question).")

(defconst rstodo-todo-type-regexp-nomark 
  (concat "\\(?:"
	  (mapconcat 'regexp-quote rstodo-todo-type-list "\\|")
	  "\\)")
  "Regexp matching all the different elements in the todo type list without
a marker.  Note that this isn't anchored to the beginning of the line.")

(defconst rstodo-todo-type-regexp
  (concat "^.?" rstodo-todo-type-regexp-nomark)
  "Regexp matching all things that can be marked completed.")

(defconst rstodo-non-todo-type-list '("##" ">>" "<<" "$$")
  "List of the beginnings of all non-todo pieces possible (not outline).")

(defconst rstodo-non-todo-type-regexp
  (concat "^\\(?:"
	  (mapconcat 'regexp-quote rstodo-non-todo-type-list "\\|")
	  "\\)")
  "Regexp matching all things that can not be marked completed.")

(defun rstodo-piece-boundaries-regexp ()
  "Regexp matching all things reserving space at finest granularity in todo file."
  (concat "\\(?:" rstodo-todo-type-regexp
	  "\\|"   rstodo-non-todo-type-regexp
	  "\\|^"  outline-regexp
	  "\\)"))

(defconst rstodo-mark-name-association 
  '(("--" . "todo") ("==" . "copy") ("##" . "note") ("$$" . "end")
    (">>" . "startr") ("<<" . "endr") ("??" . "question"))
  "Association between non-outline piece beginnings and names for them.")

(defun rstodo-beginning-of-piece ()
  "Moves point to the beginning of the current piece and returns point.
If not in a piece (i.e. in the leader), return nil."
  (end-of-line)				; Make sure to get current item
  (re-search-backward (rstodo-piece-boundaries-regexp) (point-min) t))

(defun rstodo-end-of-piece ()
  "Moves point to the end of the current piece and return point.
Note that if we're not in a piece (i.e. we're in the leader) this
function will return the end of the leader."
  (end-of-line)				; Make sure to get current item
  (if (re-search-forward (rstodo-piece-boundaries-regexp) (point-max) 'move)
      (progn (goto-char (match-beginning 0))
	     (point))
    (point)))

(defun rstodo-get-piece-info (loc)
  "Return a six element list (START END TYPE DONEP WAITP OTHERMARK) for the piece that
includes location LOC in the file.  If in the leader, type will be nil.
TYPE will be one of (todo, copy, note, end, startr, endr, outline).
DONEP (if t) indicates that the element is marked as done,
WAITP (if t) indicates that the element is dependent on something
else (both only valid for todo pieces).
OTHERMARK is any character other than 'X' before the piece prefix."
   (save-excursion
     (goto-char loc)
     (let ((start (rstodo-beginning-of-piece))
	   (end (rstodo-end-of-piece))
	   piece-prefix
	   (donep nil)
	   (waitp nil)
	   (othermark nil)
	   typename)
       (if (not start)
	   ;; Leader case
	   (list (point-min) end nil nil nil)
	 (goto-char start)
	 (if (looking-at outline-regexp)
	     ;; Outline case
	     (list start end "outline" nil nil)
	   ;; Regular cases
	   ;; Check for mark (done or other)
	   (if (looking-at (concat "^." rstodo-todo-type-regexp-nomark))
	       (progn
		 (if (looking-at "^X")
		     (setq donep t)
		   (setq othermark (char-after)))
		 (forward-char 1)))
	   ;; Get type
	   (setq piece-prefix (buffer-substring (point) (+ 2 (point)))) 
	   (setq typename 
		 (cdr (assoc piece-prefix rstodo-mark-name-association)))
	   (if (not typename)
	       (error "Couldn't recognize type of todo item; leading chars: "
		      piece-prefix))
	   (forward-char 2)
	   (re-search-forward "\\s-*")	;Should always match something
	   ;; Get Wait status
	   (if (looking-at "[(\\[]") (setq waitp t))
	   (list start end typename donep waitp othermark))))))

;;; Accessors for DS returned by above
(defun rstodo-piece-info-bounds (pi) (list (nth 0 pi) (nth 1 pi)))
(defun rstodo-piece-info-start (pi) (nth 0 pi))
(defun rstodo-piece-info-end (pi) (nth 1 pi))
(defun rstodo-piece-info-type (pi) (nth 2 pi))
(defun rstodo-piece-info-donep (pi) (nth 3 pi))
(defun rstodo-piece-info-waitp (pi) (nth 4 pi))
(defun rstodo-piece-info-othermark (pi) (nth 5 pi))

(defun rstodo-get-outline-info (loc)
  "Return a three element list (START END ELE) for the outline element that
includes location LOC in the file.  ELE will be the text of the outline 
line, minus the outline characgters.  If LOC is before the first
outline element, ELE will be nil.
Note that the location at the beginning of an outline line is within that 
outline (same as pieces)."
  (save-excursion
    (let (b e ele)
      (goto-char loc)
      (end-of-line)
      (outline-previous-heading)
      (setq b (point))
      (if (not (outline-on-heading-p t))
	  (setq ele nil)
	(goto-char (match-end 0))
	(re-search-forward "\\s-*")	;Should always match, possibly null
	(setq ele (buffer-substring (point)
				    (progn (end-of-line) (point)))))
      (outline-next-heading)
      (setq e (point))
      (list b e ele))))

;;; Accessors for DS returned by above
(defun rstodo-outline-info-bounds (oi) (list (nth 0 oi) (nth 1 oi)))
(defun rstodo-outline-info-start (oi) (nth 0 oi))
(defun rstodo-outline-info-end (oi) (nth 1 oi))
(defun rstodo-outline-info-line (oi) (nth 2 oi))

(defun rstodo-get-related-piece-info (loc rel type done wait &optional lbound ubound)
  "Return info for the piece specified relative to the piece at
LOC.  TYPE, DONE, and WAIT specify the nature of the piece being
looked for; all must match the relevant return values from
rstodo-get-piece-info.  Lists may be used in place of DONE, WAIT, and TYPE
to specify acceptable values; an empty list is interpreted as the atom nil
rather than as matching nothing.  

REL specifies how many pieces of that type forward or back to move; note that
a REL of 0 refers to the current piece, and it is an error in this case of
TYPE/DONE/WAIT do not match the current piece.  If the specified piece is
not found, NIL is returned.  LBOUND/UBOUND specify search limits; if either 
is nil, (point-min) or (point-max) will be used instead."
  ;;; Cleanup args for function
  (if (or (not type) (not (listp type)))
      (setq type (list type)))
  (if (or (not done) (not (listp done)))
      (setq done (list done)))
  (if (or (not wait) (not (listp wait)))
      (setq wait (list wait)))
  (if (not lbound) (setq lbound (point-min)))
  (if (not ubound) (setq ubound (point-max)))

  (save-excursion
    (let ((this-piece (rstodo-get-piece-info loc))
	  search-re)
      ;;; Handle specicial case of rel 0
      (if (equal rel 0)
	  (progn
	    (if (or (not (member (rstodo-piece-info-type this-piece) type))
		    (not (member (rstodo-piece-info-donep this-piece) done))
		    (not (member (rstodo-piece-info-waitp this-piece) wait)))
		(error "Current piece does not match incoming constraints.\n"
		       "Loc/rel/type/done/wait: "
		       loc "/" rel "/" type "/" done "/" wait))
	    this-piece)
	;;; General case.  First create the regexp.
	(setq search-re
	      (concat "^"
		      ;; Not done allows any other mark; don't scan for those
		      (if (and (member t done) (member nil done)) ".?")
		      (if (not (member nil done)) "X")
		      (if (not (member t done)) "[^X]?")

		      ;; For each member of type
		      ;;   I want to translate it to the prefix
		      ;;   regexp-quote it
		      ;; And then combine them with "\\(?:" .. "\\|" .. "\\)"
		      "\\(?:"
		      (mapconcat
		       '(lambda (ty) (regexp-quote
				      (car (rassoc ty rstodo-mark-name-association)))) 
		       type "\\|")
		      "\\)"
		      ;; Handle dependcy markers
		      "\\s-+"
		      (if (and (member t wait) (not (member nil wait))) "[(\\[]")
		      (if (and (member nil wait) (not (member t wait))) "[^(\\[]")
		      ))
	(goto-char (rstodo-piece-info-start this-piece))
	(if (> rel 0)
	    (progn (end-of-line)
		   (and (re-search-forward search-re ubound t rel)
			(rstodo-get-piece-info (point))))
	  ;; rel < 0
	  (and (re-search-backward search-re lbound t (- rel))
	       (rstodo-get-piece-info (point))))))))


;;; ------------  Use Case Implementation ----------------

(defun rstodo-next-todo-item (rel &optional skip-done skip-wait)
  "Move to the next todo item matching the given criteria."
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (nextpiece
	  (rstodo-get-related-piece-info
	   (point) rel '("todo" "copy" "question")
	   (if skip-done nil '(t nil))
	   (if skip-wait nil '(t nil))
	   (rstodo-outline-info-start myoutl)
	   (rstodo-outline-info-end myoutl))))
    (if nextpiece
	(goto-char (rstodo-piece-info-start nextpiece))
      (error "Couldn't find todo item."))))
  

(defun rstodo-next-open-todo-item () (interactive) (rstodo-next-todo-item 1 t nil))
(defun rstodo-prev-open-todo-item () (interactive) (rstodo-next-todo-item -1 t nil))
(defun rstodo-next-active-todo-item () (interactive) (rstodo-next-todo-item 1 t t))
(defun rstodo-prev-active-todo-item () (interactive) (rstodo-next-todo-item -1 t t))

(defun rstodo-first-active-todo-item ()
  (interactive)
  "Move to the first todo item in the section that isn't done or dependent."
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (first-active
	  (rstodo-get-related-piece-info
	   (rstodo-outline-info-start myoutl)
	   1 '("todo" "copy" "question")
	   nil nil
	   (rstodo-outline-info-start myoutl)
	   (rstodo-outline-info-end myoutl))))
    (if (not first-active)
	(error "Couldn't find active todo item in this section."))
    (goto-char (rstodo-piece-info-start first-active))))

(defun rstodo-last-active-todo-item ()
  (interactive)
  "Move to the last active todo item in this outline section."
  (let* ((outl (rstodo-get-outline-info (point)))
	 (lastpiece (rstodo-get-piece-info (rstodo-outline-info-end outl))))
    (if (and (member (rstodo-piece-info-type lastpiece)
		     '("todo" "copy" "note" "question"))
	     (not (rstodo-piece-info-waitp lastpiece))
	     (not (rstodo-piece-info-donep lastpiece)))
	(goto-char (rstodo-piece-info-start lastpiece))
      (let ((piece
	     (rstodo-get-related-piece-info
	      (rstodo-piece-info-start lastpiece)
	      -1
	      '("todo" "copy" "note" "question")
	      nil nil
	      (rstodo-outline-info-start outl)
	      (rstodo-outline-info-end outl))))
	(goto-char (rstodo-piece-info-start piece))))))

;;; Utility function for moving deleted stuff.
(defun rstodo-collect-deleted-items (loc)
  "Delete all deleted items in the outline section around LOC, 
returning them as a concatenated string."
  (let* ((myoutl (rstodo-get-outline-info loc))
	 (scanpoint (rstodo-outline-info-start myoutl))
	 (deleted-item-list '())
	 (output-string "")
	 next-item)
    (while (setq next-item
		 (rstodo-get-related-piece-info
		  scanpoint 1
		  '("todo" "copy" "question") t '(t nil)
		  (rstodo-outline-info-start myoutl)
		  (rstodo-outline-info-end myoutl)))
      (setq deleted-item-list (cons (rstodo-piece-info-bounds next-item)
				    deleted-item-list))
      ;; Note that (..related-piece-info ... 1 ...) doesn't take current item
      (setq scanpoint (rstodo-piece-info-start next-item)))
    ;; deleted-item-list is now in reverse order from the file, so deleting
    ;; in list order will not invalidate buffer locations for elements
    ;; later in the list.
    (while deleted-item-list
      (let ((ditem (car deleted-item-list)))
	(setq deleted-item-list (cdr deleted-item-list))
	(setq output-string 
	      (concat (apply 'delete-and-extract-region ditem) output-string))))
    output-string))

(defun rstodo-move-deleted-to-top (loc)
  "Move all deleted items in the outline area containing LOC to the top of that outline area."
  (interactive "d")
  (save-excursion
    (let* ((myoutl (rstodo-get-outline-info loc))
	   (outlpiece (rstodo-get-piece-info (car myoutl)))
	   deleted-items
	  )
      (goto-char (rstodo-piece-info-end outlpiece))
      (setq deleted-items (rstodo-collect-deleted-items loc))
      (insert deleted-items))))

(defun rstodo-move-item-up ()
  "Move the todo item under point one higher.
Throws an error if point is not on a todo item, or if there's no place to move it."
  (interactive)
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (outlpiece (rstodo-get-piece-info (car myoutl)))
	 (thispiece (rstodo-get-piece-info (point)))
	 (prevpiece (rstodo-get-piece-info (- (car thispiece) 1)))
	 p)
    (if (not (member (rstodo-piece-info-type thispiece)
		     '("todo" "copy" "note" "question")))
	(error (concat "Current item is of type "
		       (rstodo-piece-info-type thispiece)
		       "; it cannot be moved.")))
    (if (equal (rstodo-piece-info-start thispiece)
	       (rstodo-piece-info-end outlpiece))
	(error "Current item is already at top of list."))
    (goto-char (car prevpiece))
    (setq p (point-marker))
    (insert (apply 'delete-and-extract-region
		   (rstodo-piece-info-bounds thispiece)))
    (goto-char p)
    (set-marker p nil)))

(defun rstodo-move-item-down ()
  "Move the todo item under point one lower.
Throws an error if point is not on a todo item, or if there is no place to move it."
  (interactive)
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (outlpiece (rstodo-get-piece-info (car myoutl)))
	 (thispiece (rstodo-get-piece-info (point)))
	 (nextpiece (rstodo-get-piece-info (rstodo-piece-info-end thispiece)))
	 p)
    (if (not (member (rstodo-piece-info-type thispiece)
		     '("todo" "copy" "note" "question")))
	(error (concat "Current item is of type "
		       (rstodo-piece-info-type thispiece)
		       "; it cannot be moved.")))
    (if (equal (rstodo-piece-info-end thispiece)
	       (rstodo-outline-info-end myoutl))
	(error "Current item is already at end of list."))
    (goto-char (rstodo-piece-info-end nextpiece))
    (setq p (point-marker))
    (insert (apply 'delete-and-extract-region (rstodo-piece-info-bounds thispiece)))
    (goto-char p)
    (set-marker p nil)))

(defun rstodo-kill-todo-piece (loc)
  "Kill the todo piece which LOC is within."
  (interactive "d")
  (let ((this-piece (rstodo-get-piece-info loc)))
    (if (not (member (rstodo-piece-info-type this-piece)
		     '("todo" "copy" "note" "question")))
	(error "Invalid piece type for kill: "
	       (rstodo-piece-info-type this-piece)))
    (apply 'kill-region (rstodo-piece-info-bounds this-piece))))
  
(defvar rstodo-todo-mark (make-marker)
  "Destination marker for rstodo-move-todo-piece-to-mark.")

;; Hardcoded values below should be customizable variables.
(defun rstodo-move-todo-piece-to-mark (loc &optional copy mk)
  "Move the todo piece at LOC in the current buffer to MK.
MK defaults to rstodo-todo-mark, and may specify a different buffer.
Prefix arg indicates to copy (and mark as copy)."
  (interactive "d\nP")
  (if (not mk) (setq mk rstodo-todo-mark))
  (if (not (marker-position mk))
      (error "Destination marker not set."))
  (interactive "d")
  (save-excursion
    (let ((this-piece (rstodo-get-piece-info loc))
	  insertion-string)
      (if (not (member (rstodo-piece-info-type this-piece)
		       '("todo" "copy" "note" "question")))
	  (error "Invalid piece type for kill: "
		 (rstodo-piece-info-type this-piece)))
      (if (and copy (not (equal  (rstodo-piece-info-type this-piece) "todo")))
	  (error "Cannot copy non-todo item."))
      (if (and copy (or (rstodo-piece-info-donep this-piece)
			(rstodo-piece-info-othermark this-piece)))
	  (error "Cannot copy marked todo item."))
      (if copy	  ;Assume todo
	  (progn 
	    ;; Should be controlled to be length of todo prefix.
	    (goto-char (+ (rstodo-piece-info-start this-piece) 2))
	    (if (equal (char-after) ?>)
		(progn (forward-char)
		       (if (equal (char-after) ?!)
			   (forward-char))))
	    (setq insertion-string
		  (concat "=="
			  (buffer-substring (point)
					    (rstodo-piece-info-end this-piece)))))
	(setq insertion-string (apply 'delete-and-extract-region
				      (rstodo-piece-info-bounds this-piece))))
      (set-buffer (marker-buffer mk))
      (goto-char (marker-position mk))
      (insert-before-markers insertion-string))))

(defun rstodo-set-todo-mark (loc &optional buf)
  "Set the todo mark to LOC (default point).
The todo marker is used as a destination for 
rstodo-move-todo-piece-to-mark."
  (interactive "d")
  (if (not buf)
      (setq buf (current-buffer)))
  (set-marker rstodo-todo-mark loc buf)
  (message "Todo mark set."))

(defun rstodo-collect-outline-topics ()
  "Return a list of information about each outline topic in the current buffer.
 (XXX specify format.)"
  (save-excursion
    (let* ((current-outline (rstodo-get-outline-info (point-min)))
	   (current-outline-end (rstodo-outline-info-end current-outline))
	   (outline-list (list current-outline))
	  )
      (while (not (equal current-outline-end (point-max)))
	(setq current-outline (rstodo-get-outline-info current-outline-end))
	(setq current-outline-end (rstodo-outline-info-end current-outline))
	(setq outline-list (append outline-list (list current-outline))))
      outline-list)))

(defconst rstodo-hotkey-regexp "\\[[a-zA-Z]\\(,[a-zA-Z]\\)*\\]"
  "Regular expression for finding hotkeys in outline topic headings.")

(defun rstodo-collect-outline-hotkeys ()
  "Returns the list of outline topics marked with hotkeys.
An outline topic is marked with a hotkey if it matches the regexp 
\"\[[a-zA-Z]\(,[a-zA-Z]\)*\]\"."
  (let ((full-outline-list (rstodo-collect-outline-topics))
	hotkey-outline-list)
    (while full-outline-list
      (let* ((current-outline (car full-outline-list))
	     (outline-heading (rstodo-outline-info-line current-outline))
	     tmpkeylist hotkeys)
	(setq full-outline-list (cdr full-outline-list))
	(if (and outline-heading
		 (string-match rstodo-hotkey-regexp outline-heading))
	    (progn
	      (setq tmpkeylist (substring outline-heading
					  (1+ (match-beginning 0))
					  (1- (match-end 0))))
	      (setq hotkeys (mapcar '(lambda (s) (elt s 0))
				    (split-string tmpkeylist ",")))
	      (setq hotkey-outline-list
		    (append hotkey-outline-list
			    (list
			     (append current-outline (list hotkeys)))))))))
    hotkey-outline-list))

(defun rstodo-outline-hotkey-cheatsheet ()
  "Return a text listing of the mapping between hotkeys and sections."
  (let ((hotkey-list (rstodo-collect-outline-hotkeys)))
    (mapconcat 'identity 
	       (sort (apply 'append
			    (mapcar '(lambda (elt)
				       (let ((line (nth 2 elt))
					     (keys (nth 3 elt)))
					 (mapcar '(lambda (key)
						    (concat (string key) " --\t" line))
						 keys)))
				    hotkey-list))
		     'string<) "\n")))

;;; Plan
;;;	* Create cheatsheet
;;;	* Display cheatsheet in buffer *cheatsheet*.  Make window
;;;	  the "other" window against the current one, and make it's size
;;;	  the smaller of the numbers of lines in the cheatsheet and
;;;	  half the frame.
;;;	* Prompt for letter
;;;	* Nuke buffer and cheatsheet window
;;;	* Go to beginning of section mentioned and expand it; put top
;;;	  of section at top of page.
;;;
;;; ???: Should I get the section by interactive propmt?
;;; ???: Refactor so that cheatsheet is created from hotkey list?
;;; ???: Need to check for hotkey conflicts somewhere.  
(defun rstodo-goto-outline-section-by-hotkey ()
  "Go to an outline section specified by hotkey."
  (interactive)
  (let* ((cheatsheet (rstodo-outline-hotkey-cheatsheet))
	 (hotkeys (rstodo-collect-outline-hotkeys))
	 (cb (get-buffer-create "*cheatsheet*"))
	 char-read
	 buffer-line-length
	)
    (save-excursion
      (set-buffer cb)
      (insert cheatsheet)
      (setq buffer-line-length (count-lines (point-min) (point-max)))
      (switch-to-buffer-other-window cb t)
      (fit-window-to-buffer (get-buffer-window cb))
      (setq char-read (read-char "Section to visit: "))
      (delete-window (get-buffer-window cb))
      (kill-buffer cb))
    (while hotkeys
      (if (member char-read (nth 3 (car hotkeys)))
	  (progn
	    (goto-char (nth 0 (car hotkeys)))
	    (show-entry)
	    (recenter 0)
	    (setq hotkeys nil)
	    (end-of-line)
	    (rstodo-first-active-todo-item))
	(setq hotkeys (cdr hotkeys))
	(if (not hotkeys)
	    (error "Key '%s' not found bound to outline heading."
		   (char-to-string char-read)))))))

(define-derived-mode rstodo-mode outline-mode "Todo"
  "Major mode for todo lists in Randy style.
\\{rstodo-mode-map}"
  (setq outline-font-lock-faces
    [outline-2 outline-1 outline-3 outline-4
	       outline-5 outline-6 outline-7 outline-8]))

(fset 'rstodo-insert-prioritize
   [return ?\C-p ?> ?> ?\S-  ?P ?r ?i ?o ?t ?i backspace backspace ?r ?i ?t ?i ?z ?e return return ?< ?< return ?\C-p ?\C-p return return ?\C-p])


;;; Binding to C-c <blank>.  Outline stuff:
;;;	C-a make all text visible
;;;	C-b backward same level
;;;	C-d hide subtree
;;;	C-f forward same level
;;;	C-n next visible heading
;;;	C-p previous visible heading
;;;	C-s show subtree
;;;	C-t make all text invisible
;;;	C-u up heading


;;;	C-q make only the first N levels of headers visible
;;;	TAB show children
;;;	C-c make body invisible
;;;	C-e make body visible
;;;	C-l make descendent bodies invisible
;;;	C-k make all subheadings visible

;;; To bind:
;;;	next-open-todo-item		right
;;;	prev-open-todo-item		left
;;;	next-active-todo-item		down
;;;	prev-active-todo-item		up
;;;	move-deleted-to-top		C-x
;;;	move-item-up			^
;;;	move-item-down			.
(define-key rstodo-mode-map [?\C-c right] 'rstodo-next-active-todo-item)
(define-key rstodo-mode-map [?\C-c left] 'rstodo-prev-active-todo-item)
(define-key rstodo-mode-map [?\C-c down] 'rstodo-last-active-todo-item)
(define-key rstodo-mode-map [?\C-c up] 'rstodo-first-active-todo-item)
(define-key rstodo-mode-map [?\C-c ?\C-x] 'rstodo-move-deleted-to-top)
(define-key rstodo-mode-map "\C-c\C-k" 'rstodo-kill-todo-piece)
(define-key rstodo-mode-map [?\C-c ?\C-\s] 'rstodo-set-todo-mark)
(define-key rstodo-mode-map "\C-c\C-j" 'rstodo-goto-outline-section-by-hotkey)

(define-key rstodo-mode-map [f5] 'rstodo-move-todo-piece-to-mark)
(define-key rstodo-mode-map [f6] 'rstodo-next-active-todo-item)
(define-key rstodo-mode-map [f7] 'rstodo-move-item-up)
(define-key rstodo-mode-map [f8] 'rstodo-move-item-down)

(provide 'rs-todo)
