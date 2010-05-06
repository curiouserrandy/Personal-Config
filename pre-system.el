;;; Variables to modify emacs' behavior, non-locale specific
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(setq find-file-visit-truename t)
(setq version-control t)		; Always make numeric backups
(setq require-final-newline 1)		; Ask when saving
(setq-default case-fold-search nil)
(setq delete-old-versions t) ; Almost never use backup; standard fine.

(defvar tags-revert-without-query)	; Defined in etags.el
(setq tags-revert-without-query t)

(if (load "default" t t)
    (setq inhibit-default-init t))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'mail-mode-hook
	  '(lambda ()
	     (auto-fill-mode 1)
	     (setq fill-prefix "  ")
	     (setq mail-yank-prefix "> ")))

(fset 'randy-copy-shell-env-def
   [?\C-a ?\C-k escape escape ?: ?( ?s ?e ?t ?e ?n ?v ?  ?\" ?\C-y ?\" ?) ?\C-r ?= ?\C-m ?\" ?\C-d ?  ?\" return])

;;; Tools I always want available
(require 'rs-split)
(require 'rs-grepplusplus)
(require 'rs-outline)
(require 'rs-draw)			; Sets up but does not enable draw mode
(require 'rs-structure)
(require 'rs-underpoint)
(require 'rs-processtalk)
(require 'rs-buffernames)
(require 'rs-todo)

