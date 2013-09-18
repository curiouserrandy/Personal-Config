# Environ variables
case $EDITOR in
    emacs*|*emacsclient*)	# Leave it alone
    	;;
    *)
        export EDITOR=emacs
	;;
esac

# Figure out who am I, as best is possible.

if test "$USER" != "" ; then
    LOGNAME=$USER;
elif test "$LOGNAME" != "" ; then
    USER=$LOGNAME;
fi
export LOGNAME USER

# Clean up DISPLAY, if not fully qualified (can cause problems)
# So can cleaning it up; nuking this for now.
# dhost=`echo $DISPLAY | awk -F: '{print $1;}'`
# dd=`echo $DISPLAY | awk -F: '{print $2;}'`
# if ! echo "$dhost" | grep '\.' > /dev/null; then
#     # It's ok if it's localhost
#     if ! [ X"$dhost" = X"localhost" ]; then
#         # If there isn't a fully qualified hostname, qualify it.
#         export DISPLAY="$fqhname:$dd";
#     fi
# fi
#
# unset dhost dd

# See post-system.bf for use.
shell_prompt_commands=""
