;;; Allow setting of environ variables for contexts in which emacs
;;; isn't being started from a shell that's run the users .bashrc.
(let ((envfile (concat (getenv "HOME") "/.emacs_env")))
  (if (file-exists-p envfile)
      (load envfile)))

;;; Redirect to root of configuration tree
(if (not (getenv "config_files_directory"))
    (error "Environmental variable \"config_files_directory\" not defined"))

(load (concat (getenv "config_files_directory") "/all"))
