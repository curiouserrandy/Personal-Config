;;; My keymap rebindings.  Everything goes here, so that I can see conflicts
;;; at a top level; i.e. this file is heavily conditionalized.

(require 'rs-text-abbrev)

;;; Todo
;;;	* Figure out how to deal with gdb mode hook function @ end of file.
;;;	  May not be necessary because of comint mode.

;;; Global maps (i.e things I should't be doing :-})
(define-key esc-map ":" 'goto-line)
(define-key esc-map "?" 'what-line)
(define-key esc-map "+" 'pop-tag-mark)	;Deprecate--it's on M-*
(define-key global-map "\C-z" nil)	;Causes too much grief.
(if (equal "Darwin" randy-configuration-os)
    (progn
      (define-key esc-map " " 'just-one-space)   ;Reset back to correct value.
      (add-hook 'dired-mode-hook
		'(lambda () (define-key dired-mode-map ":"
			      '(lambda ()
				 (interactive)
				 (dired-do-shell-command
				  "open" current-prefix-arg
				  (dired-get-marked-files t current-prefix-arg))))))))
						      
  

;;; The "official" user customization keys; \C-c[a-z]
;;; a
;;; b
(if (featurep 'chrome-dev)
    (define-key mode-specific-map "b" 'chrome-visit-issue))
(define-key mode-specific-map "c" 'compile)
(add-hook 'shell-mode-hook
	  '(lambda () (define-key shell-mode-map "\C-cd"
			'randy-shell-cd-directory-other-window)))
(define-key mode-specific-map "e" '(lambda () (interactive)
				     (insert "** ")
				     (shell-command "date" t)
				     (end-of-line)))
(define-key mode-specific-map "f" 'randy-alternative-find-file-under-point)
(define-key mode-specific-map "g" 'grep)
(define-key mode-specific-map "h" 'shell)
(if (featurep 'rs-tools-git)
    ;; Git overrides; better functionality.
    (define-key mode-specific-map "i" 'randy-run-git-grep)
  (define-key mode-specific-map "i" 'randy-run-glimpse))
(define-key mode-specific-map "j" 'randy-run-egrep-recursively)
(add-hook 'comint-mode-hook
	  '(lambda ()
	     (define-key comint-mode-map "\C-ck\C-a" 'randy-telnet-send-break)
	     (define-key comint-mode-map "\C-ck\C-c" 'randy-telnet-quit)))
(define-key mode-specific-map "l" '(lambda () (interactive)
				    (setq truncate-lines
				     (not truncate-lines))))
(define-key mode-specific-map "m" 'randy-format-man-page)
(if (featurep 'rs-netapp)
    (define-key mode-specific-map "n" 'randy-find-burt))
(define-key mode-specific-map "o" 'overwrite-mode)
(define-key mode-specific-map "p" 'copy-rectangle-to-register)
(define-key mode-specific-map "q" 'auto-fill-mode)

(define-key mode-specific-map "r"
  '(lambda () (interactive) (revert-buffer t t)))
(define-key mode-specific-map "s" 'randy-change-default-sandbox)
(define-key mode-specific-map "t" 'randy-query-change-name); pneumonic: t-xlate
(define-key mode-specific-map "u" 'randy-uniquename-buffer)
(define-key mode-specific-map "v" 'set-vi-type-indent)
(define-key mode-specific-map "w" 'write-region)
(if (featurep 'rs-frames)
    (progn
      (define-key mode-specific-map "xs" 'randy-save-current-config)
      (define-key mode-specific-map "xr" 'randy-restore-config)
      (define-key mode-specific-map "xd" 'randy-delete-named-config)))
  
;;; x
;;; y
(define-key mode-specific-map "z" 'randy-greplist-sandbox-file)
(add-hook 'rstodo-mode-hook
	  '(lambda ()
	     (define-key outline-mode-map "\C-cz" 'rstodo-insert-prioritize)))

;;; Other non-control keys following \C-c
(define-key mode-specific-map ">" '(lambda () (interactive) (randy-shift-font 1)))
(define-key mode-specific-map "<" '(lambda () (interactive) (randy-shift-font -1)))
(define-key mode-specific-map "^" 'randy-set-window-height)
(define-key mode-specific-map "%" 'query-replace-regexp)

;;; Control keys following \C-c
;; (define-key mode-specific-map "\C-?" 'backward-kill-sexp)
;; (define-key mode-specific-map "\C-k" 'kill-sexp)
;; (define-key mode-specific-map "." 'find-yacc-tag)
(if (featurep 'rs-comint)
    (add-hook 'comint-mode-hook
	      '(lambda () 
		 (define-key comint-mode-map "\C-c\C-c" 'send-unbuffered-break)
		 (define-key comint-mode-map "\C-c\C-q" 'randy-send-unbuffered-string))))
(add-hook 'mail-mode-hook
	  '(lambda () 
	     (define-key mail-mode-map "\C-c\C-r" 'randy-include-signature)))

;;; Function key definitions
(define-key global-map '[f12] 'quoted-tab)
(define-key global-map '[f11] 'randy-restore-config)
(define-key global-map '[f9] 'next-error)

;;; Mouse key defns
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 2)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 2)))
(global-set-key [double-mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [double-mouse-5] '(lambda () (interactive) (scroll-up 5)))
(global-set-key [triple-mouse-4] '(lambda () (interactive) (scroll-down 8)))
(global-set-key [triple-mouse-5] '(lambda () (interactive) (scroll-up 8)))

;;; --

(defun randy-gdb-mode-hook-function ()
    (define-key (current-local-map) "\C-c\C-q" 'randy-send-unbuffered-string))



