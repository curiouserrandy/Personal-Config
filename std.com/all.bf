# This is a tricky one.  In general, we assume that this is a ppp connection
# to world and we're on tigana.  The one exception to this is if we're
# actually on world.  I.e. these are basically tigana specific aliases.

if [ "`hostname | sed 's/\..*$//'`" = "world" ]; then
    # To be filled in.
    /bin/true;
else
    if [ "`hostname | sed 's/\..*$//'`" = "tigana" ]; then
	/bin/true;	# Will be taken care of at top level.
    else
	. $configuration_files_directory/std.com/tigana.bf
    fi
fi
