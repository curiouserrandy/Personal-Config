(fset 'quoted-tab [?\C-q tab])

(defun randy-include-signature ()
  (interactive)
  (insert "

						-- Randy

"))

(provide 'rs-text-abbrev)
