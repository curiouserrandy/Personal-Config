;;; Function to send a ^C to a process without following it with a
;;; newline.
(defun send-unbuffered-break ()
  (interactive)
  (let ((process (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process"))))
    (process-send-string process "")))

(defun randy-send-unbuffered-string (string)
  "Send a arbitrary string to process with no newline at the end."
  (interactive "sString to send: ")
  (let ((process (or (get-buffer-process (current-buffer))
		     (error "Current buffer has no process"))))
    (process-send-string process string)))

(defun randy-shell-cd-directory-other-window (pushd-p)
  "Cd to the current directory of the other window on the screen.
With prefix arg, does a pushd rather than a cd.
Only valid in a shell window."
  (interactive "P")
  (let (cddir (cmd (if pushd-p "pushd" "cd")))
    (other-window -1)
    (setq cddir default-directory)
    (other-window 1)
    (goto-char (point-max))
    (insert cmd " \"" cddir "\"")
    (comint-send-input)))

(defun randy-telnet-quit ()
  (interactive)
  (randy-send-unbuffered-string "")
  (randy-send-unbuffered-string "quit
"))

(defun randy-telnet-send-break ()
  (interactive)
  (randy-send-unbuffered-string "")
  (randy-send-unbuffered-string "send break
"))

;;; Use ssh, not rsh
(setq remote-shell-program "ssh")

(provide 'rs-processtalk)
