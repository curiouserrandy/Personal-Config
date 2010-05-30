# Directory below which all configuration files live.
# Default the best you can; otherwise use in environment (for su root)
if [ "$config_files_directory" == "" ]; then
    export config_files_directory=~/Config
fi

# Don't execute this file is the shell running is not bash.
# This can happen if someone insists on executing a non-interactive shell
# not based on the shell variable.  I set ENV to this file, and if the
# shell executed is a bourne shell variant, it will pick that up.

if [ "$BASH" != "" ]; then
    . $config_files_directory/master.bf;
fi

