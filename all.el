;;; Master emacs loading file.  
;;; Does very bare-bones setup (functions needed for the rest of the
;;; file), pulls in configuration environment variables, and executes
;;; per-system files (parallel to the all.bf system).

;;; Note that unlike the rest of the initialization, this file 
;;; isn't automatically byte-compiled when it's out of date.  Any
;;; changes to this file should involve byte compiling.

(defun randy-assert-file-ok (filename)
  (if (not (file-exists-p filename))
      (error "File %s does not exist" filename))
  (if (not (file-readable-p filename))
      (error "File %s is not readable" filename))
  (let ((fa (file-attributes filename)))
    (if (not fa)
	(error "Attributes for file %s could not be read" filename))
    (if (eq (car fa) t)
	(error "File %s is a directory" filename))
    (if (car fa)
	(error "File %s is a symbolic link" filename))))

(defun compile-if-newer-then-load (file)
  "Load the file specified (possibly compiling first)."
  (let ((elfile (concat file ".el"))
	(elcfile (concat file ".elc")))
    (if (file-exists-p elfile)
	(progn 
	  (randy-assert-file-ok elfile)
	  (if (file-exists-p elcfile)
	      (randy-assert-file-ok elcfile))
	  (if (file-newer-than-file-p elfile elcfile)
	      (byte-compile-file elfile))
	  (load elcfile)))))

;;; Backwards compatibility.
(defvar randy-configuration-directory (getenv "config_files_directory")
  "The directory under which all configuration files can be found.")
(defvar randy-configuration-host (getenv "config_host")
  "The bare (not FQ) hostname of the current host.")
(defvar randy-configuration-domain (getenv "config_domain")
  "The domainname (without hostname) of the current host.")
(defvar randy-configuration-os (getenv "config_os")
  "The operating system type of the current host.")
(defvar randy-configuration-arch (getenv "config_arch")
  "The hardware archiecture of the current host.")
(defvar randy-configuration-fhost (getenv "force_host")
  "The host that we should pretend to be (dealing with DHCP weirdnesses)")
(defvar randy-configuration-fdomain (getenv "force_domain")
  "The domain that we should pretend to be (dealing with DHCP weirdnesses)")

(defun randy-init-directly (argconcat)
  (let* ((dirspecdp (file-directory-p argconcat))
	 (filebase (cond (dirspecdp (concat argconcat "/all"))
			 ((equal (substring argconcat -3) ".el")
			  (substring argconcat 0 -3))
			 ((equal (substring argconcat -4) ".elc")
			  (substring argconcat 0 -4))
			 (t argconcat))))
    (if (and dirspecdp (not (member argconcat load-path)))
	(setq load-path (append load-path (list argconcat))))
    (compile-if-newer-then-load filebase)))
	 
(defun randy-init-from (subfile)
  (randy-init-directly (concat randy-configuration-directory "/" subfile)))

(randy-init-from "Emacs")
(randy-init-from "pre-system")

;;; Read in everything the shell initiatlization has told us to
(let* ((files (getenv "emacs_init_list"))
       (file-list (split-string files ":")))
  (while file-list
    (randy-init-directly (car file-list))
    (setq file-list (cdr file-list))))

(randy-init-from "Emacs/rs-persist")
(randy-init-from "Emacs/rs-frames")
(randy-init-from "Emacs/rs-compile")
(randy-init-from "Emacs/rs-server")
(randy-init-from "Emacs/rs-man")
(randy-init-from "post-system")
(randy-init-from "Emacs/rs-keys")	; Keyboard mappings

;; Unilaterally setup emacs server; I think this is an ok place for that.
(rs-server-start)			;Sets EDITOR in the environment.

;; Unilaterally enable winner mode.
(winner-mode t)

(shell)
