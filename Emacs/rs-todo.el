;;; Functions for manipulating outline files used as todo files.

;;; Current hotkey bindings:
;;	* ^C ^R to enter
;;	* <letter>: Move todo item to section
;;	* <del>: Delete todo item under point
;;	* <backspace>: Mark done
;;	* F5: Next todo item
;;	* F9: Move item to todo mark.
;;	* ^C# to exit.
;;; I think ^C^R would be more natural for exit, and F5 and F9 really
;;; should be next to each other.

;;; See notes in Todomode.txt for design and format spec.

(require 'rs-outline)

(load-library "outline")		;Needed for outline-regexp

;;; ------------  Infrastructure implementation ----------------

;; This starts todo items.  They end at this regexp (next todo item),
;; outline-regexp, or the end of the file.
(defconst rstodo-todo-type-list '("--" "==" "??" "~~")
  "List of the types that a todo item may have (should do, can do, question, and non-authoritative).")

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
    (">>" . "startr") ("<<" . "endr") ("??" . "question")
    ("~~" . "nonauthoritative"))
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

;; TODO(rdsmith): Replace all uses of get-piece-info with the below
;; collection (specifically get-item-boundaries).
(defun rstodo-item-beginning (loc)
  "Returns the location of the beginning of the item at LOC, or nil if not in
an item."
  (save-excursion
    (goto-char loc)
    (rstodo-beginning-of-piece)
    (if (looking-at (concat "\\(?:" rstodo-todo-type-regexp
			    "\\|" rstodo-non-todo-type-regexp "\\)"))
	(point)
      nil)))

(defun rstodo-item-end (loc)
  "Returns the location of the end of the item at LOC.
This function assumes LOC is in an item; use rs-todo-beginning-of-item
for detemrining item existence."
  (save-excursion
    (goto-char loc)
    (rstodo-end-of-piece)
    (point)))

(defun rstodo-item-boundaries (loc)
  "Return a two element list (START END) for the item identified by LOC.
Note that a LOC at the very beginning of a piece (== START) will return
that piece.  If location is not within a item (beginning of file or
outline section), nil will be returned."
  (save-excursion
    (goto-char loc)
    (let ((bounds ((rstodo-beginning-of-item) (rstodo-end-of-item))))
      ;; nil if no start, otherwise bounds.
      (and (car bounds) (bounds)))))

(defun rstodo-item-type (loc)
  "Return the type (from rstodo-mark-name-association) of the item at LOC."
  (let ((start (rstodo-beginning-of-item loc)))
    (and start
	 (save-excursion
	   (goto-char start)
	   (if (or (looking-at
		    (concat ".?\\(" rstodo-todo-type-regexp-nomark "\\)"))
		   (looking-at
		    (concat "\\(" rstodo-non-todo-type-regexp "\\)")))
	       (assoc (cdr
		       (buffer-substring (match-beginning 1) (match-end 1))
		       rstodo-mark-name-association))
	     (error "Todo item at %d cannot be parsed!?" loc))))))

(defun rstodo-item-mark (loc)
  "Return the mark (if any) of the todo item at LOC."
  (let ((start (rstodo-beginning-of-item loc)))
    (and start
	 (save-excursion
	   (goto-char start)
	   (if (or (looking-at
		    (concat "\\(.?\\)" rstodo-todo-type-regexp-nomark))
		   (looking-at
		    (concat "\\(\\)" rstodo-non-todo-type-regexp)))
	       (buffer-substring (match-beginning 1) (match-end 1))
	     (error "Todo item at %d cannot be parsed!?" loc))))))

(defun rstodo-item-donep (loc)
  "Return whether the item at LOC is completed."
  (equal "X" (rstodo-get-mark loc)))

(defun rstodo-item-waitp (loc)
  "Return whether or not the todo item at LOC is waiting on something."
  (let ((start (rstodo-beginning-of-item loc)))
    (and start
	 (save-excursion
	   (goto-char start)
	   (looking-at (concat rstodo-todo-type-regexp-nomark
			       "[ 	][ 	]*[(\\[]"))))))
;;;;;;;;;

(defun rstodo-get-outline-info (loc)
  "Return a three element list (START END ELE) for the outline element that
includes location LOC in the file.  ELE will be the text of the outline
line, minus the outline characters.  If LOC is before the first
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

(defun rstodo-get-outline-beginning (loc)
  "Return the beginning of the outline element that contains LOC.
If LOC is before the first outline element, the return value
will be nil."
  (save-excursion
    (goto-char loc)
    (outline-previous-heading)
    (if (outline-on-heading-p t)
	(point)
      nil)))

(defun rstodo-get-outline-end (loc)
  "Return the end of the outline element that contains LOC."
  (save-excursion
    (goto-char loc)
    (outline-next-heading)
    (point)))

;;; Accessors for DS returned by above
(defun rstodo-outline-info-bounds (oi) (list (nth 0 oi) (nth 1 oi)))
(defun rstodo-outline-info-start (oi) (nth 0 oi))
(defun rstodo-outline-info-end (oi) (nth 1 oi))
(defun rstodo-outline-info-line (oi) (nth 2 oi))

(defun rstodo-get-related-item-beginning (loc rel type done wait
					      &optional lbound ubound)
  "Return the beginning of the item at offset REL from location LOC.
TYPE, DONE, and WAIT specify the nature of the item being looked for;
REL only counts items matching these criteria.

If moving forward, the search begins from the end of the current line;
if moving backwards, from the beginning.  This allows the function
to work properly if at the beginning of an item.   A REL of
0 is an invalid input.

If the specified item is not found, NIL is returned.
LBOUND/UBOUND specify search limits; if either is
nil, (point-{min,max}) will be used instead."
  ;;; Cleanup args for function
  (if (equal rel 0)
      (error "Relative location of 0 specified to rstodo-get-related-item-beginning"))
  (if (or (not type) (not (listp type)))
      (setq type (list type)))
  (if (or (not done) (not (listp done)))
      (setq done (list done)))
  (if (or (not wait) (not (listp wait)))
      (setq wait (list wait)))
  (if (not lbound) (setq lbound (rstodo-get-outline-beginning loc)))
  (if (not ubound) (setq ubound (rstodo-get-outline-end loc)))

  ;; Create regexp
  (let ((search-re
	 (concat "^"
		 ;; Done allow only ^X, not done ^[^X ^I]?, both ^[^ ^I]
		 ;; Writing this as ^\\(?:X\\|[^X ^I]?\\)
		 "\\(?:"
		 (if (member t done) "X")
		 (if (and (member t done) (member nil done)) "\\|")
		 (if (member nil done) "[^X 	]?")
		 "\\)"

		 ;; Map types to prefix and quote.
		 "\\(?:"
		 (mapconcat
		  (lambda (ty) (regexp-quote
				(car (rassoc ty rstodo-mark-name-association))))
		  type "\\|")
		 "\\)"

		 "[ 	]+"

		 ;; Check dependency marker.
		 "\\(?:"
		 (if (member t wait) "(")
		 (if (and (member t wait) (member nil done)) "\\|")
		 (if (member nil wait) "[^(]")
		 "\\)")))
    (save-excursion
      (if (> rel 0)
	  (progn (end-of-line)
		 (and (re-search-forward search-re ubound t rel)
		      (progn (beginning-of-line)
			     (point))))
	(progn (beginning-of-line)
	       (and (re-search-backward search-re lbound t (- rel))
		    (point)))))))

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
not found, NIL is returned.

LBOUND/UBOUND specify search limits; if either is nil, the beginning/end
of the outline unit around point will be used insted."
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
		      ;; Not done allows any other mark (other than whitespce); don't scan for those
		      (if (and (member t done) (member nil done)) ".?")
		      (if (not (member nil done)) "X")
		      (if (not (member t done)) "[^X 	]?")

		      ;; For each member of type
		      ;;   I want to translate it to the prefix
		      ;;   regexp-quote it
		      ;; And then combine them with "\\(?:" .. "\\|" .. "\\)"
		      "\\(?:"
		      (mapconcat
		       (lambda (ty) (regexp-quote
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

(defun rstodo-next-todo-item (rel &optional skip-done skip-wait skip-note)
  "Move to the next todo item matching the given criteria."
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (nextitem
	  (rstodo-get-related-item-beginning
	   (point) rel (append '("todo" "copy" "question" "nonauthoritative")
			       (if skip-note '() '("note")))
	   (if skip-done nil '(t nil))
	   (if skip-wait nil '(t nil)))))
    (if nextitem (goto-char nextitem)
      (error "Couldn't find todo item."))))

(defun rstodo-first-active-todo-item ()
  (interactive)
  "Move to the first todo item in the section that isn't done or dependent."
  (let* ((myoutl (rstodo-get-outline-info (point)))
	 (first-active
	  (rstodo-get-related-item-beginning
	   (rstodo-outline-info-start myoutl)
	   1 '("todo" "copy" "question") nil nil)))
    (if (not first-active)
	(message "Couldn't find active todo item in this section.")
      (goto-char first-active))))

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
		  '("todo" "copy" "question" "nonauthoritative") t '(t nil)
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

;;; TODO(rdsmith): Test the next three functions!
(defun rstodo-remove-todo-item (loc)
  "Remove the todo item LOC is within, returning its text.
The beginning of the item counts as within."
  (let ((item-beginning (rstodo-item-beginning loc))
	(item-end (rstodo-item-end loc))
	contents)
    (if (not item-beginning)
	(error "Location %d isn't within an item" loc))
    (setq contents (buffer-substring item-beginning item-end))
    (delete-region item-beginning item-end)
    contents))

(defun rstodo-collect-deleted-items-1 (loc)
  (let* ((outline-info (rstodo-get-outline-info loc))
	 (outline-beginning (rstodo-outline-info-start outline-info))
	 (output-string "")
	 tmp)
    (while
	(setq tmp
	      (rstodo-get-related-item-beginning
	       outline-beginning 1
	       ("todo" "copy" "question" "nonauthoritative") t '(t nil)))
      (setq output-string (concat output-string (rstodo-remove-todo-item tmp))))
    output-string))

(defun rstodo-kill-deleted-items (loc)
  "Kill all deleted items (i.e. put them all in the kill ring)."
  (kill-new (rstodo-collect-deleted-items-1 loc)))

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

(defun rstodo-move-deleted-to-end (loc)
  "Move all deleted items in the outline area containing LOC to the end of that outline area."
  (interactive "d")
  (save-excursion
    (let* ((myoutl (rstodo-get-outline-info loc))
	   deleted-items
	  )
      (goto-char (car (cdr myoutl)))
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

;; Saving to make a subroutine.
(defun rstodo-kill-todo-piece (loc)
  "Kill the todo piece which LOC is within."
  (interactive "d")
  (let ((this-piece (rstodo-get-piece-info loc)))
    (if (not (member (rstodo-piece-info-type this-piece)
		     '("todo" "copy" "note" "question" "nonauthoritative")))
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
		       '("todo" "copy" "note" "question" "nonauthoritative")))
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

(defconst rstodo-hotkey-regexp-1 "\\[\\([a-zA-Z0-9!@#\\$%\\^&\\*()]\\)\\]"
  "Regular expression for finding hotkeys in outline topic headings.")

(defun rstodo-hotkey-outline-info-list ()
  "Return a list of information about each tagged outline header in the buffer.
A list member will look like '(tag beginning end header-string)."
  (let ((tagged-outline-list nil)
	(tagged-outline-regexp (concat "^" outline-regexp
				       "[ 	]*\\(.*?\\)[ 	]*"
				       rstodo-hotkey-regexp-1
				       "[ 	]*[\\-\\+]?[ 	]*$")))
    (outline-map-region
     '(lambda ()
	(if (looking-at tagged-outline-regexp)
	    (let ((tag (buffer-substring-no-properties (match-beginning 2)
						       (match-end 2)))
		  (heading (buffer-substring-no-properties (match-beginning 1)
							   (match-end 1))))
	      (setq tagged-outline-list
		    (cons (list tag (point)
				(save-excursion (outline-next-heading) (point))
				heading)
			  tagged-outline-list)))))
     (point-min) (point-max))
    ;; TODO(rdmsith): ??? Sort is not doing nothing nor sorting ???
    (sort tagged-outline-list
	  #'(lambda (a b) (string< (car a) (car b))))))

(defun rstodo-hotkey-outline-info (key)
  (assoc key (rstodo-hotkey-outline-info-list)))

(defun rstodo-move-current-todo-item-to-hotkey (key)
  (interactive "cDestination Section: ")
  (setq key (make-string 1 key))
  (message key)
  (let ((target-section (rstodo-hotkey-outline-info key)) m)
    (if (not target-section)
	(let* ((header (read-from-minibuffer "Section heading: "))
	       (level (save-excursion
			(outline-back-to-heading t)
			(funcall outline-level)))
	       (full-line (concat (make-string (1+ level) ?*)
				  " " header " [" key "]" [10])))
	  (save-excursion
	    (outline-next-heading)
	    (insert full-line))
	  (setq target-section (rstodo-hotkey-outline-info key))
	  (if (not target-section)
	      (error "Can't happen--just created target section."))))
    (setq m (copy-marker (nth 1 target-section)))
    (let ((todo-contents (rstodo-remove-todo-item (point))))
      (save-excursion
	(goto-char m)
	(forward-line 1)
	(insert todo-contents)))
    (set-marker m nil)))

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

(defconst rstodo-hotkey-regexp "\\[[a-zA-Z0-9!@#\\$]\\(,[a-zA-Z0-9!@#\\$]\\)*\\]"
  "Regular expression for finding hotkeys in outline topic headings.")

(defun rstodo-collect-outline-hotkeys ()
  "Returns the list of outline topics marked with hotkeys.
An outline topic is marked with a hotkey if it matches the regexp
\"\[[a-zA-Z0-9]\(,[a-zA-Z0-9]\)*\]\"."
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
	      (setq hotkeys (mapcar (lambda (s) (elt s 0))
				    (split-string tmpkeylist ",")))
	      (setq hotkey-outline-list
		    (append hotkey-outline-list
			    (list
			     (append current-outline (list hotkeys)))))))))
    hotkey-outline-list))

(defun rstodo-outline-hotkey-cheatsheet ()
  "Return a text listing of the mapping between hotkeys and sections."
  (let ((hotkey-list (rstodo-collect-outline-hotkeys)))
    (concat "<backspace> --\tMark entry as done
<del> --\tRemove entry
"
    (mapconcat 'identity
	       (apply 'append
		      (mapcar (lambda (elt)
				(let ((line (nth 2 elt))
				      (keys (nth 3 elt)))
				  (mapcar (lambda (key)
					    (concat (string key) " --\t\t" line))
					  keys)))
			      hotkey-list)) "\n"))))

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
    (while (and hotkeys (not (member char-read (nth 3 (car hotkeys)))))
      (setq hotkeys (cdr hotkeys)))
    (if (not hotkeys)
	(error "Key '%s' not found bound to outline heading."
	       (char-to-string char-read)))
    (goto-char (nth 0 (car hotkeys)))
    (show-entry)
    (recenter 0)
    (end-of-line)
    (rstodo-first-active-todo-item))
  (message (concat (buffer-name (current-buffer)) ": At point %d") (point))
)

(defun rstodo-setup-cheatsheet-buffer ()
  "Setup the buffer containing the cheetsheet.
Returns buffer; does not display it."
  (save-excursion
    (let ((cb (get-buffer-create "*cheatsheet*"))
	  (cheatsheet (rstodo-outline-hotkey-cheatsheet)))
      (set-buffer cb)
      (delete-region (point-min) (point-max))
      (insert cheatsheet)
      cb)))

;;; Take current todo item and put it in a project file; insert link
;;; to project file at current location.
;;;	* Error checking around todo item.
;;;	* Determine if space ended or not.
;;;	* Prompt user for project file
;;; 	* Confirm project file can be opened.
;;;	* Put Todo template into it.
;;;	* Kill current todo item into project file.
;;;	* Insert link to project file in place.

(defun rstodo-extract-piece (loc)
  "Delete the piece at loc and return it from this function"
  (apply 'delete-and-extract-region
	 (rstodo-piece-info-bounds (rstodo-get-piece-info loc))))

(defun rstodo-mark-piece-done (loc)
  "Mark the piece at loc as done."
  (save-excursion
    (goto-char (rstodo-piece-info-start (rstodo-get-piece-info loc)))
    (insert "X")))

(defun rstodo-move-item-to-self-section ()
  (interactive)
  (if (not (equal (length (this-command-keys)) 1))
      (error (concat "this-command-keys has length greater than 1: "
		     this-command-keys)))
  (rstodo-move-current-todo-item-to-hotkey (elt (this-command-keys) 0))
  (rstodo-setup-cheatsheet-buffer))

(defun rstodo-move-item-to-section-prompting (section-character)
  (interactive "cSection: ")
  (rstodo-move-current-todo-item-to-hotkey section-character))

;;; Managing completion list by day

(defconst rstodo-completion-file "TimeLog"
  "File into which completed elements will be moved.
Directory is the same as the current file.")

(defconst rstodo-completion-file-init
  "## -*-mode: outline; fill-prefix: \"   \";-*-

"
  "Initial contents of completion file.")

(defun rstodo-init-completion-file ()
  "Return a buffer visiting the completion file, initialized appropriately
(i.e. with initial contents if empty, with a line for todays date if not
previously created)"
  (let ((file-buffer (find-file-noselect rstodo-completion-file))
	(current-date-string
	 (concat "* " (format-time-string "%a %y/%m/%d") "\n")))
    (save-excursion
      (set-buffer file-buffer)
      ;; Make sure it's an outline file
      (if (equal (point-min) (point-max))
	  (insert rstodo-completion-file-init))
      (goto-char (point-min))
      ;; Make sure it's got today's date at the end of it.
      (if (not (search-forward current-date-string (point-max) 1))
	  (insert current-date-string))
      (auto-save-visited-mode t)
    file-buffer)))

;;; TODO: Expand to work on items not indicated by cursor?
(defun rstodo-item-to-completion-file ()
  (interactive)
  (let ((current-piece (rstodo-extract-piece (point))))
    (save-excursion
      (set-buffer (rstodo-init-completion-file))
      (goto-char (point-max))
      (insert current-piece))))

(defun rstodo-move-deleted-to-completion-file (loc)
  "Move all completed (X--...) items in the outline area containing LOC 
to the completion file."
  (interactive "d")
  (save-excursion
    (let* ((myoutl (rstodo-get-outline-info loc))
	   deleted-items
	  )
      (setq deleted-items (rstodo-collect-deleted-items loc))
      (set-buffer (rstodo-init-completion-file))
      (goto-char (point-max))
      (insert deleted-items))))

;;; Starting a new daily entry

;;; TODO: Turn this template into a file.
(defconst rstodo-new-entry 
  "## Schedule:

-- Create new planning chunk:   
	* Create new planning chunk & fill in things that are top of mind.  
	* Fill in schedule for planning chunk
	* Go over top level categories and fill in what foci will be.
	* Clean out email
	* Sort out staging section.  (C-c C-r for hotkey mode.)
	* Previous planning chunks -> current, [i], elsewhere
	* Pull in stuff from subsections as appropriate for foci.
	* Sort through new chunk

-- New planning chunk, alterantive path:
	* Filter ->i all items in [D] that aren't important to keep TOM.
	* Add in stuff to [D] that you want top of mind.   
	* Fill in schedule for planning chunk
	* Clean out email
	* Go over Foci and think about chunk in terms of it.
	* Sort out staging section.
	* Sort through new chunk


## Foci:
	* Purpose/protein: 
	* Big projects driving: 
	* Small Life maintenance: 
	* Health/self-care: 
	* Social:
	* Exercise:

*** Self-care [!] +

*** Life maintenance [@] +

*** Fun (including social) [#] +

*** Work [$] +

"
  "Initial contents for new daily entry")

;;; TODO: Refactor rstodo-goto-outline-section-by-hotkey to break out
;;; finding the section by a given hotkey and use below.
(defun rstodo-start-new-daily-entry ()
  (interactive)
  (let (target) 
    (save-excursion ;; Will change point after if not error.
      ;; Goto [D] section and remove [D]
      (goto-char (point-min))
      (if (re-search-forward "^\\*.*\\[D\\]" (point-max) t) ; Ok if no [D]
	  (delete-char -3))

      ;; Goto end of "Scheduled" section and insert heading.
      (goto-char (point-min))
      (re-search-forward "^\\* Scheduled[ 	]*$")
      (outline-forward-same-level 1)
      (insert (concat "** "
		      (substring (shell-command-to-string "date +\"%a %m/%d\"")
				 0 -1)
		      " [D] +\n\n" rstodo-new-entry))
      (setq target (point)))

    (goto-char target)))

;;; TODO(rdsmith): Next two mappings (entry and exit into hotkey mode)
;;; have a "show-entry" terminating them.  This is a hack to get around
;;; the fact that I hide-body when entering outline mode (see rs-outline.el).
;;; The right solution here is to make hotkey mode a minor mode rather than
;;; a major one, so the major mode doesn't change.

;;; *** rstodo-mode-map definition
(define-derived-mode rstodo-mode outline-mode "Todo"
  "Major mode for todo lists in Randy style.
\\{rstodo-mode-map}"
  (setq outline-font-lock-faces
    [outline-2 outline-1 outline-3 outline-4
	       outline-5 outline-6 outline-7 outline-8]))

(define-key rstodo-mode-map [?\C-c ?\C-x]
  'rstodo-move-deleted-to-completion-file)
(define-key rstodo-mode-map [?\C-c ?\C-\s] 'rstodo-set-todo-mark)
(define-key rstodo-mode-map "\C-c\C-j" 'rstodo-goto-outline-section-by-hotkey)
(define-key rstodo-mode-map "\C-cn" 'rstodo-start-new-daily-entry)

(define-key rstodo-mode-map [f5]
 (lambda () (interactive) (rstodo-next-todo-item 1 t nil)))
(define-key rstodo-mode-map [M-f5]
 (lambda () (interactive) (rstodo-next-todo-item 1 t nil t)))
(define-key rstodo-mode-map [C-f5] 'rstodo-move-item-down)

(define-key rstodo-mode-map [f6]
 (lambda () (interactive) (rstodo-next-todo-item -1 t nil)))
(define-key rstodo-mode-map [M-f6]
 (lambda () (interactive) (rstodo-next-todo-item -1 t nil t)))
(define-key rstodo-mode-map [C-f6] 'rstodo-move-item-up)

(define-key rstodo-mode-map [f7] 'rstodo-item-to-completion-file)

(define-key rstodo-mode-map [f8] 'rstodo-move-item-to-section-prompting)

(define-key rstodo-mode-map [f9] 'rstodo-move-todo-piece-to-mark)

;;; Reset the buffer back to specified layout.
;;; With prefix argument, actually do a revert.
(define-key rstodo-mode-map "\C-cr" (lambda (really-revert)
				      (interactive "P")
				      (if really-revert
					  (revert-buffer t t)
					(randy-explode-hook)
					(randy-implode-hook))))

(define-key rstodo-mode-map "\C-c\C-r"
  #'(lambda ()
      (interactive)
      (let ((cb (rstodo-setup-cheatsheet-buffer)))
	(switch-to-buffer-other-window cb t)
	(other-window 1)
	(rstodo-hotkey-mode)
	(show-entry))))

;;; *** rstodo-hotkey-mode-map definition
(define-derived-mode rstodo-hotkey-mode rstodo-mode "Todo *Hotkey*"
  "Mode in which keys auto-move the current todo item to that heading.")

(define-key rstodo-hotkey-mode-map "\C-c\C-c"
  #'(lambda ()
      (interactive)
      (let* ((csb (get-buffer "*cheatsheet*"))
	     (csbw (and csb (get-buffer-window csb))))
	(if csbw (delete-window csbw))
	(if csb (kill-buffer csb)))
      (rstodo-mode)
      (show-entry)))

(define-key rstodo-hotkey-mode-map [remap self-insert-command]
  'rstodo-move-item-to-self-section)

(define-key rstodo-hotkey-mode-map (kbd "<deletechar>")
  #'(lambda ()
      (interactive)
      (rstodo-extract-piece (point))))

(define-key rstodo-hotkey-mode-map [backspace] 'rstodo-item-to-completion-file)

(provide 'rs-todo)
