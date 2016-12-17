(defun function-occur ()
  "List the functions defined in a C file."
  (interactive)
  (occur "^[a-zA-Z_].*(\\($\\|.*[^;
]$\\)" nil))

(setq c-basic-offset 2)

(defun randy-visit-corresponding-source-file ()
  "Visit the file corresponding to the current one (.h <-> .cc)"
  (interactive)
  (let ((current-filename (buffer-file-name)))
    (cond
     ((string-equal ".cc" (substring current-filename -3 nil))
      (find-file (concat (substring current-filename 0 -2) "h")))
     ((string-equal ".cpp" (substring current-filename -4 nil))
      (find-file (concat (substring current-filename 0 -3) "h")))
     ((string-equal ".h" (substring current-filename -2 nil))
      (find-file (concat (substring current-filename 0 -1) "cc")))
     (t
      (message (concat "Unrecognized source file type: " current-filename))))))

(provide 'rs-programs)
