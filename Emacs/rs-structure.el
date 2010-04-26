;;; Todo:
;;;	* Make box functionality should work in non-picture mode (overwrite)

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

;;; XXX: There are functions in picture-mode for making boxes; I should
;;; probably use htem instead of the below.

;;; Details: Put mark at one corner, and point at the other,
;;; and do "\C-c\C-r" (picture-draw-rectangle) and a rectangle will
;;; be drawn through those points, overwriting anything in the way.

;;; I think it's worthwhile to adapt that function over to making a box around
;;; the current word(s).  But that's a separate thing from what I used to
;;; have, so I'll delete it.


(provide 'rs-structure)
