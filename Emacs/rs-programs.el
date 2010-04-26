(defun function-occur ()
  "List the functions defined in a C file."
  (interactive)
  (occur "^[a-zA-Z_].*(\\($\\|.*[^;
]$\\)" nil))

(setq c-basic-offset 4)

(provide 'rs-programs)
