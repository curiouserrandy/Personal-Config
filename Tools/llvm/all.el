(autoload 'lldb "gud-lldb"
  "Run lldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory 
and source-file directory of your debugger."
  t)

(defun lldb-auto-wrap (binary proc_id)
  (require 'gud-lldb)
  (lldb (concat "lldb " binary))
  (gud-call (concat "attach " (int-to-string proc_id)))
  (gud-call "continue"))
