# /opt/local/bin goes earlier because it sometimes has new versions for
# things that are in the system directories.
prefix_path_with_dirlist PATH /opt/local/bin 
suffix_path_with_dirlist PATH /Developer/Tools/ /opt/local/sbin
suffix_path_with_dirlist MANPATH /opt/local/man

# Darwin specific tools
init_from $config_files_directory/Tools/fink
init_from $config_files_directory/Tools/MacPorts
