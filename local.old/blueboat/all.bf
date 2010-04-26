umask 002			# To allow sharing between randy/rsmith

# Put X windows stuff on the path.
suffix_path_with_dirlist PATH /usr/X11R6/bin
suffix_path_with_dirlist MANPATH /usr/X11R6/man

init_from $configuration_files_directory/Tools/glimpse

# Deal with ssh agent
tmpsock="`echo /tmp/ssh*/ag*`";
if [ "$tmpsock" != 'echo /tmp/ssh*/ag*' ]; then
    export SSH_AUTH_SOCK="$tmpsock";
fi

# Often I have an X window server running but am not running from within 
# it (e.g. Mac Emacs).  Still, I'd like to have ssh work with X.  So 
# I enable DISPLAY proactively.
export DISPLAY=:0.0
