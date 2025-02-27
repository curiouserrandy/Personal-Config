# /opt/local/bin goes earlier because it sometimes has new versions for
# things that are in the system directories.
prefix_path_with_dirlist PATH /opt/local/bin /opt/local/libexec/gnubin
suffix_path_with_dirlist PATH /Developer/Tools/ /opt/local/sbin
suffix_path_with_dirlist MANPATH /opt/local/man
suffix_path_with_dirlist MANPATH /Applications/Xcode.app/Contents/Developer/usr/share/man
suffix_path_with_dirlist MANPATH /Applications/Emacs.app/Contents/Resources/man

# So that we find the right emacsclient 
prefix_path_with_dirlist PATH /Applications/Emacs.app/Contents/MacOS/bin/

# Darwin specific tools
init_from $config_files_directory/Tools/fink
init_from $config_files_directory/Tools/MacPorts
