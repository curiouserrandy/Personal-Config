;;; Redirect to root of configuration tree

(if (not (getenv "config_files_directory"))
    (error "Environmental variable \"config_files_directory\" not defined"))

(load (concat (getenv "config_files_directory") "/all"))
