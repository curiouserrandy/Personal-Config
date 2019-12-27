;;; My keymap rebindings.  Everything goes here, so that I can see conflicts
;;; at a top level; i.e. this file is heavily conditionalized.

(require 'rs-text-abbrev)

;;; Todo
;;;	* Figure out how to deal with gdb mode hook function @ end of file.
;;;	  May not be necessary because of comint mode.

;;; Global maps (i.e things I should't be doing :-})
(define-key esc-map ":" 'goto-line)
(define-key esc-map "?" 'what-line)
;;(define-key esc-map "+" 'pop-tag-mark)	;Deprecate--it's on M-*
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
(define-key mode-specific-map "c" 'compile)
(if (featurep 'randy-citc)
    (progn
      (define-key mode-specific-map "C" 
	'(lambda () (interactive) (if current-prefix-arg
				      (randy-buildclean-blaze-compile-current-file)
				    (randy-blaze-compile-current-file))))))

(if (featurep 'rs-google-envoy)
    (progn
      (define-key mode-specific-map "b" 'envoy-visit-speciality)))

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
;; j
(if (featurep 'rs-google-envoy)
    (define-key mode-specific-map "k" 'randy-find-checkout))
;; l  (Used to be truncate-lines toggle)
(if (featurep 'rs-dirhist)
    (define-key mode-specific-map "m" 'randy-rotate-dired-history))
;; n
;; o
(define-key mode-specific-map "p" 'copy-rectangle-to-register)
;; q
(define-key mode-specific-map "r"
  '(lambda () (interactive) (revert-buffer t t)))
;; s
(define-key mode-specific-map "t" 'randy-query-change-name); pneumonic: t-xlate
(define-key mode-specific-map "u" 'randy-uniquename-buffer)
(define-key mode-specific-map "v" 'set-vi-type-indent)
(define-key mode-specific-map "w" 'write-region)
(if (featurep 'rs-frames)
    (progn
      (define-key mode-specific-map "xs" 'randy-save-current-config)
      (define-key mode-specific-map "xr" 'randy-restore-cycle-config)
      (define-key mode-specific-map "xd" 'randy-delete-named-config)))
;; y
;; z
(add-hook 'rstodo-mode-hook
	  '(lambda ()
	     (define-key outline-mode-map "\C-cz" 'rstodo-insert-prioritize)))

;;; Other non-control keys following \C-c
(define-key mode-specific-map "^" 'randy-visit-corresponding-source-file)
(define-key mode-specific-map "%" 'query-replace-regexp)

;;; Control keys following \C-c
(if (featurep 'envoy-dev)
    (define-key mode-specific-map [?\C-.] 'envoy-codesearch))

;; Move iconification off of S-m; I hit it too often.
(global-set-key [(super ?m)] 'undefined)

;;; Function key definitions
(define-key global-map '[f12] 'quoted-tab)
(define-key global-map '[f11] 'randy-restore-cycle-config)
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

;; Making available for remote executions that may need it.
(defun randy-flip-meta ()
  (interactive)
  (progn (setq x-meta-keysym 'alt) (setq x-alt-keysym 'meta)))


