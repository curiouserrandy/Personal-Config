;;; "Draw mode", where arrow keys and number move around leaving behind
;;; the currently specified character

(require 'picture)

(defvar rs-draw-moveinsert-char ?#
  "Character to be inserted by rs-draw-insert-and-move.")

(defconst rs-draw-lines ["\\|/"
			  "-+-"
			  "/|\\" ]
  "Characters to use in drawing if drawing character unspecified")

(defun rs-draw-insert-and-move (hstep vstep)
  (let ((picture-vertical-step vstep)
	(picture-horizontal-step hstep)
	(local-char (or rs-draw-moveinsert-char
			(aref (aref rs-draw-lines (+ vstep 1)) (+ hstep 1)))))
    (picture-update-desired-column t)
    (picture-insert local-char 1)))

(defun rs-draw-set-char (drawing-char)
  "Set draw mode using character DRAWING-CHAR.
If DRAWING-CHAR is nil, the best guesses for lines in the directions moved
are used (prefix arg will produce this behavior)."
  (interactive (and current-prefix-arg
		    (read-char "Char for draw mode: ")))
  (setq rs-draw-moveinsert-char drawing-char))

;;; Setup the keymap for this mode.
;;; Setup keymap
(defvar rs-draw-map nil "Keymap for draw mode bindings.")
(if (not rs-draw-map)
    (progn 
      (setq rs-draw-map (make-sparse-keymap))
      (let ((numkey-layout ["123"
			    "456"
			    "789"])
	    (arrow-layout '((left -1 0)
			    (right 1 0)
			    (up 0 -1)
			    (down 0 1))))
	(let ((x -1) (y -1))
	  (while (and (<= x 1) (<= y 1))
	    (define-key rs-draw-map
	      (string (aref (aref numkey-layout (+ y 1)) (+ x 1)))
	      `(lambda () (interactive) (rs-draw-insert-and-move ,x ,y)))
	    (if (< x 1)
		(setq x (+ x 1))
	      (setq y (+ y 1))
	      (setq x -1))))
	(dolist (arrow-map arrow-layout)
	  (define-key rs-draw-map (vector (car arrow-map))
	    `(lambda () (interactive) (rs-draw-insert-and-move
				       ,(nth 1 arrow-map)
				       ,(nth 2 arrow-map)))))
	(define-key rs-draw-map "\C-c#" 'rs-draw-set-char))))

(define-minor-mode rs-draw-mode
  "Toggle draw mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When draw mode is enabled, the arrow keys and number keys (as per the usual 
numeric keypad map) will write a character into the current buffer position 
before moving.  That character will be the value of RS-DRAW-MOVEINSERT-CHAR
if it is set, or the modes best guess for an appropriate character if it
isn't set.
\\[rs-draw-set-char] will set the drawing character."
  nil " draw" rs-draw-map)

(provide 'rs-draw)
