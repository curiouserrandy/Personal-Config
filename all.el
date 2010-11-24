;;; Master emacs loading file.  
;;; Does very bare-bones setup (functions needed for the rest of the
;;; file), pulls in configuration environment variables, and executes
;;; per-system files (parallel to the all.bf system).

;;; Note that unlike the rest of the initialization, this file 
;;; isn't automatically byte-compiled when it's out of date.  Any
;;; changes to this file should involve byte compiling.

;;; All of the following environmental variables are setup by my
;;; normal startup.  Their setup is a requirement for proper
;;; operation of this file.
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

(defun randy-init-from (&rest args)
  "Load the file (possibly compiling first) identified by the routine arguments.
The arguments represent elements of the pathname to the file to be compiled,
rooted at the directory specified by the variable randy-configuration-directory.  
If the args specify a directory, the directory will be included in the load
path and the file \"all\" in that directory will be loaded.  
The suffix .elc will be appended to the path to find the actual file; if that
file is older than the corresponding .el file, the .el will be compiled.
It's ok for this function to be called pointing into space."
  (if (not randy-configuration-directory)
      (error "Root directory variable RANDY-CONFIGURATION-DIRECTORY not defined in function randy-init-from"))
  (let* ((argconcat (apply 'concat randy-configuration-directory 
			(mapcar '(lambda (pe) (concat "/" pe)) args)))
	 (dirspecdp (file-directory-p argconcat))
	 (filebase (cond (dirspecdp (concat argconcat "/all"))
			 ((equal (substring argconcat -3) ".el")
			  (substring argconcat 0 -3))
			 ((equal (substring argconcat -4) ".elc")
			  (substring argconcat 0 -4))
			 (t argconcat)))
	 (elfile (concat filebase ".el"))
	 (elcfile (concat filebase ".elc")))
    ;;; Usage of this function may point at something that doesn't exist;
    ;;; that's ok.
    (if (and dirspecdp (not (member argconcat load-path)))
	(setq load-path (append load-path (list argconcat))))
    (if (file-exists-p elfile)
	(progn 
	  (randy-assert-file-ok elfile)
	  (if (file-exists-p elcfile)
	      (randy-assert-file-ok elcfile))
	  (if (file-newer-than-file-p elfile elcfile)
	      (byte-compile-file elfile))
	  (load elcfile)))))

(if (not (and randy-configuration-directory
	      randy-configuration-host
	      randy-configuration-domain
	      randy-configuration-os
	      randy-configuration-arch))
    (error "Incomplete environment for initialization."))

;;; The standard stuff; echoes all.bf.  randy-init-from prefixes
;;; randy-configuration-directory, and concats args with "/" between
(randy-init-from "Emacs")
(randy-init-from "pre-system")
(randy-init-from "OS" randy-configuration-os)
(randy-init-from "OS" randy-configuration-os randy-configuration-arch)

(let ((tmp-config-domain randy-configuration-domain))
  (while (not (equal tmp-config-domain ""))
    (message (concat "Loading from directory " tmp-config-domain))
    (randy-init-from tmp-config-domain)
    (randy-init-from tmp-config-domain "OS" randy-configuration-os)
    (randy-init-from tmp-config-domain
		     "OS"
		     randy-configuration-os
		     randy-configuration-arch)
    (randy-init-from tmp-config-domain randy-configuration-host)
    (setq tmp-config-domain
	  (mapconcat 'identity
		     (cdr (split-string tmp-config-domain "\\.")) "."))))
  
(if randy-configuration-fdomain
    (progn
      (randy-init-from randy-configuration-fdomain)
      (randy-init-from randy-configuration-fdomain "OS" randy-configuration-os)
      (randy-init-from randy-configuration-fdomain
		       "OS"
		       randy-configuration-os
		       randy-configuration-arch)
      (if randy-configuration-fhost
	  (randy-init-from randy-configuration-fdomain
			   randy-configuration-fhost))
      ))
(randy-init-from "Emacs/rs-persist")
(randy-init-from "Emacs/rs-frames")
(randy-init-from "Emacs/rs-compile")
(randy-init-from "post-system")
(randy-init-from "Emacs/rs-keys")	; Keyboard mappings

;; Unilaterally setup emacs server; I think this is an ok place for that.
(server-start)
(setenv "EDITOR" "emacsclient")
