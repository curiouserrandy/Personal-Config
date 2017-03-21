;;; This file contains the extra definitions necessary to use lldb with GUD. 
;;; If it's ever useful to upstream it, it should be merged into gud.el, but
;;; for now keeping it separate (and not adding it to the various lists
;;; of debuggers in that file) seems more useful.  This does mean that 
;;; there are commands that will not show in the gud menus.  

;;; This file is based on and intended to be used with the gud.el in 
;;; Emacs 23.3.1, as well as on
;;; https://llvm.org/viewvc/llvm-project/lldb/trunk/utils/emacs/gud.el?view=log&pathrev=122286
;;; (the last gud.el committed to the lldb tree).

(require 'gud)

;; History of argument lists passed to lldb.
(defvar gud-lldb-history nil)

(defun gud-lldb-marker-filter (string)
  (setq gud-marker-acc
	(if gud-marker-acc (concat gud-marker-acc string) string))
  (let (start)
    ;; Process all copmlete markers in this chunk
    (while (or
	    ;; thread/process stop.
	    (string-match " at \([^:\n]*\):\([0-9]*\), stop reason = .*\015?\n"
			  gud-marker-acc start)
	    ;; frame up/down
	    (string-match "frame.* at \\([^:\n]*\\):\\([0-9]*\\)\015?\n"
			  gud-marker-acc start))
      (setq gud-last-frame
            (cons (match-string 1 gud-marker-acc)
                  (string-to-number (match-string 2 gud-marker-acc)))
            start (match-end 0)))

    ;; Consume to last newline.
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))
    (setq gud-marker-acc (substring gud-marker-acc (or start 0)))
    string))

;;;###autoload
(defun lldb (command-line)
  "Run lldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory 
and source-file directory of your debugger."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line nil 'gud-lldb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'lldb)

  ;; Make lldb dump fullpath instead of basename for a file.
  ;; See also gud-lldb-marker-filter where gud-last-frame is grokked from lldb output.
  ;; TODO(rdsmith): Maybe have it dump both basename and fullname, and elide fullname
  ;; from output so that the user doesn't have to deal with it?
  (progn
    (gud-call "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n")
    (gud-call "settings set thread-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n")
    (gud-call "settings set stop-line-count-before 0")
    (gud-call "settings set stop-line-count-after 0"))

  ;; Define all commands
  (gud-def gud-listb  "breakpoint list"
	   "l"    "List all breakpoints.")
  (gud-def gud-bt     "thread backtrace"
	   "b"    "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"
	   "B"    "Show stacks for all the threads.")

  (gud-def gud-break 	"breakpoint set -f %f -l %l"
	   "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak	"breakpoint set -f %f -l %l -o"
	   "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove	"breakpoint clear -f %f -l %l"
	   "\C-d" "Remove breakpoint at current line")

  (gud-def gud-print	"expression -- %e"
	   "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar	"expression -- *%e"
	   nil    "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-down	"frame select -r -1"
	   ">"    "Down 1 stack frame.")
  (gud-def gud-up	"frame select -r 1"
	   "<"    "Up 1 stack frame.")

  (gud-def gud-run	"run"
	   "r"    "Run the program.")
  (gud-def gud-cont	"process continue"
	   "\C-r" "Continue with display.")
  (gud-def gud-until	"thread until %l"
	   "\C-u" "Continue to current line.")
  (gud-def gud-finish	"thread step-out"
	   "\C-f" "Finish executing current function.")
  ;; TODO(rdsmith): Check if this also needs a temporary breakpoint
  ;; (i.e. if underlying functionality jumps and goes.)
  (gud-def gud-jump	"thread jump -l %l -f %f"
	   "\C-j" "Set execution address to current line.")
  (gud-def gud-next	"thread step-over"
	   "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti	"thread step-inst-over"
	   nil    "Step one instruction (skip functions).")
  (gud-def gud-step	"thread step-in"
	   "\C-s" "Step one source line with display.")
  (gud-def gud-stepi	"thread step-inst"
	   "\C-i" "Step one instruction with display.")

  (gud-def gud-stop-subjob    "process kill"
                      "s"    "Stop the program.")
  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook))

;;; TODO(rdsmith): Things that are lacking because I'm not hacking gud.el directly; might want
;;; to find a way to add.
;;;	* gud-tooltip-print-command
;;;	* Visibility of various menu items
;;;	* Documentation in comint-mode.  

(provide 'gud-lldb)
