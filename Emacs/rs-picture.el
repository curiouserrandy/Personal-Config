(autoload 'picture-yank-rectangle-from-register "picture"
	  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
	  t)

(provide 'rs-picture)

