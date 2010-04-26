umask 002			# To allow sharing between randy/rsmith

# Put X windows stuff on the path.
suffix_path_with_dirlist PATH /usr/X11R6/bin
suffix_path_with_dirlist MANPATH /usr/X11R6/man

# Deal with ssh agent
# tmpsock="`echo /tmp/ssh*/ag*`";
# if [ "$tmpsock" != 'echo /tmp/ssh*/ag*' ]; then
#     export SSH_AUTH_SOCK="$tmpsock";
# fi

# Use emacsclient as the editor, since we don't always have X
# Server will be started in all.el in this directory
export EDITOR=emacsclient

init_from $configuration_files_directory/Tools/macports
