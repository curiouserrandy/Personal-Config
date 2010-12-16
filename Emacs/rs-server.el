(require 'server)

(defun rs-server-start (&optional leave-dead)
  "Start an emacs server as server-start, but with a name based
on the process number, and put that name into the environment variable
EMACS_SERVER_NAME for subprocesees of emacs."
  (interactive "P")
  (let ((server-name (format "EmacsServer%d" (emacs-pid))))
    (setenv "EDITOR" (concat "emacsclient -s " server-name))
    (server-start leave-dead)))
    
(provide 'rs-server)
