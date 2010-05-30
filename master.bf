# Read in functions intended to be used as tools by the startup process.
. $config_files_directory/config-tools.bf

# We try to get a path for host before reaming out the default path.  
# This may be necessary on certain wonky systems (like cygwin).
host_path=`type -p host`;

# I don't trust "host" commands in /usr/local/bin
if [ X"$host_path" = X"/usr/local/bin/host" ]; then
    host_path=
fi

# Create a path that we intend will, for all machines, have the 
# following executables on it:
# 	uname, hostname, host, awk, sed
PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/ucb:/etc:/usr/etc
export PATH

# Get the fully qualified hostname and the domain name.
# The head -1 is to deal with situations in which the hostname is set
# to a multi-line string (multiple responses to a DNS PTR request).
hostname=`uname -n | head -1`
if echo $hostname | grep '\.' > /dev/null ; then
    fqhname=`echo $hostname | sed 's/\.$//'`;
else
    if [ X"$host_path" != X"" ]; then
	fqhname=`$host_path $hostname | awk '{print $1}' | sed 's/\.$//'`
    else
	fqhname=`nslookup $hostname | awk '/^Name:/ {print $2}'`;
    fi
    if [ "$fqhname" = "" ]; then
	fqhname=$hostname;
    fi
fi

hostname=`echo $fqhname | awk -F. '{print $1}' | tr '[A-Z]' '[a-z]'`
domainname=`echo $fqhname | sed 's/^[^\.]*\.//' | tr '[A-Z]' '[a-z]'`

if [ "$domainname" = "" ]; then
    domainname=NODOMAIN
fi

# Get the system type and machine type. 
# Protect against systypes with "/"s (BSD/OS, specifically)
systype=`uname | sed 's;/;_;'`
archtype=`uname -m | sed -e 's;/;_;' -e 's; ;_;' `

# Do any overriding of the above configuration variables (e.g. for laptops
# using DHCP; generally you want those to have a fixed environment no matter
# where they're hooked up).
. $config_files_directory/config-override.bf

# Do a reasonable default for the various paths.  They will get
# modified by the various files read in below, using
# {prefix,suffix}_path_with_dirlist.  Thus the latter files may choose 
# to override the earlier ones, or not.
PATH=""
suffix_path_with_dirlist PATH /usr/bin /bin /usr/sbin /sbin /usr/games
export PATH

MANPATH=""
suffix_path_with_dirlist MANPATH /usr/man /usr/share/man
export MANPATH

# Not sure if this should have a default.  
LD_LIBRARY_PATH=""
#suffix_path_with_dirlist LD_LIBRARY_PATH /usr/lib /lib 
export LD_LIBRARY_PATH

# Some systems use LIBPATH rather than LD_LIBRARY_PATH, so replicate.
export LIBPATH=$LD_LIBRARY_PATH

# Get local versions of directories onto path, at the beginning in
# case there are replacements there to override.
prefix_path_with_dirlist PATH /usr/local/bin /usr/local/sbin 
prefix_path_with_dirlist MANPATH /usr/local/man
suffix_path_with_dirlist PATH /opt/local/bin /opt/local/sbin
suffix_path_with_dirlist MANPATH /opt/local/man /opt/local/share/man

# Get my personal stuff that needs to come in before machine specific files
. $config_files_directory/pre-system.bf

# Do the configuration initialization
init_from_recurse $config_files_directory

# Do project-specific configuration

if [ -r ~/.current_project.bf ]; then
    source ~/.current_project.bf
fi

# Get my personal stuff that needs to come in after machine specific files
. $config_files_directory/post-system.bf

# Setup environment variables for other tools (e.g. emacs) that want to 
# do setups like the above.
export config_files_directory=$config_files_directory;
export config_host=$hostname;
export config_domain=$domainname;
export config_os=$systype;
export config_arch=$archtype;
