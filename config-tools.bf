# Add a value (in $1) to a variable named in $2.  $2 names a colon separated
# list of strings, usually a directory list.  If the contents of the variable
# in $2 is null, it is set equal to $1.
function suffix_val_to_var ()
{
    eval "deref="'$'"$2";
    if [ "$deref" = "" ]; then
	eval "$2=$1";
    else
        eval "$2="'$'"$2:$1";
    fi
}

function suffix_val_to_var_if_not_present ()
{
    eval "deref="'$'"$2";
    if ! fgrep $1 <(echo $deref | tr ':' '\012') > /dev/null ; then
        suffix_val_to_var $1 $2
    fi
}

function prefix_val_to_var ()
{
    eval "deref="'$'"$2";
    if [ "$deref" = "" ]; then
	eval "$2=$1";
    else
        eval "$2=$1:"'$'"$2";
    fi
}

# Add directories to paths, if they exist and are searchable.
# Directory in $1, path variable name in $2
function suffix_dir_to_path ()
{
    if [ -d $1 -a -x $1 ]; then
        suffix_val_to_var $1 $2
    fi
}

function prefix_dir_to_path ()
{
    if [ -d $1 -a -x $1 ]; then
        prefix_val_to_var $1 $2
    fi
}

# Go through a list of directories, either prefixing or suffixing them
# onto the variable (passed as $1) as requested.  Note that the prefix
# operation will reverse the order, as the arguments are processed left
# to right.  Note further that the directories are only added if they exist 
# and are searchable, but that you can add the same directory multiple 
# times.  
function prefix_path_with_dirlist ()
{
    dir=$1;
    while [ $# -gt 1 ] ; do
	shift;
        prefix_dir_to_path $1 $dir;
    done
}

function suffix_path_with_dirlist ()
{
    dir=$1;
    while [ $# -gt 1 ] ; do
	shift;
        suffix_dir_to_path $1 $dir;
    done
}

# Ways to remove directories from path
function remove_dir_from_path ()
{
    dir=$1;
    path=$2;
    
    echo $path | tr ':' '\012' | egrep -v '^'"${dir}"'$' | tr '\012' ':' | sed 's/:$//'
}

function absolute_pathname ()
{
    if [ $# -ne 1 ]; then
        echo "absolute_pathname called with wrong number of args: $*" 1>&2;
	return 1;
    fi
    
    case $1 in
    /*) echo $1;
        ;;
    *) echo $PWD/$1;
       ;;
    esac
    return 0;
}    
    
read_if_exists ()
{
    if [ $# -ne 1 ]; then
        echo "read_if_exists called with wrong number of args: $*" 1>&2;
	return 1;
    fi

    if [ ! -e $1 ]; then
        return 0;		# Fine for it not to exist.
    elif [ ! -f $1 ]; then
        echo "read_if_exists: argument is not a file: $1" 2>&1;
	return 1;
    elif [ ! -r $1 ]; then
        echo "read_if_exists: argument is not readable: $1" 2>&1;
	return 1;
    else
        . $1;
    fi
    return 0;
}

# Source $1/all.bf and put $1/Bin on the path.  No error if anything
# ($1, $1/all.br, $1/Bin) doesn't exist, but if they exist they must be
# a directory/readable/searchable as appropriate.
function init_from ()
{
    if [ $# -ne 1 ]; then
        echo "init_from called with more than one arg: $*" 1>&2;
	return 1;
    fi
    
    local file_root=$1;
    
     if [ -e ${file_root} ]; then
         # Confirm a directory, then read <dir>/all.bf if exists, and
	 # put <dir>/Bin on path if it exists.
        if [ ! -d ${file_root} ]; then
	    echo "File ${file_root} isn't a directory!" 2>&1;
	    return 1;
	elif [ ! -x ${file_root} ]; then
	    echo "Directory $file_root isn't searchable!" 2>&1;
	    return 1;
	else
	    if [ "$INIT_FROM_LOG" = "yes" ]; then echo "IFing directory ${file_root}"; fi
	    # It's a searchable directory.  Put the Bin on the path.
	    prefix_path_with_dirlist PATH `absolute_pathname ${file_root}`/Bin;
	    
	    # Source the all.bf file.
	    read_if_exists ${file_root}/all.bf;

	    suffix_val_to_var_if_not_present ${file_root} emacs_init_list
	    export emacs_init_list
	fi
    else
	if [ "$INIT_FROM_LOG" = "yes" ]; then echo "IFing ${file_root} failed"; fi
    fi
}

# Execute init_from on the first argument, then recurse into standard 
# subdirectories.  The standard subdirectories are:
#	OS/$systype
#	OS/$systype/$archtype
#	<hostname-no-domain>
#	Each contiguous domain suffix (e.g. edut, mit.edu,
#	    bcs.mit.edu, glab.bcs.mit.edu)
# Note that this relies on the variables $systype, $archtype,
# and $domainname being set.
#
# Given an infnite directory structure this could lead to infinite recursion. 
# Given a non-infnite directory structure, a directory's lack of existence 
# will cause the function to return without recursing.  Assuming a
# non-infinite directory structure seems safe :-}.
# 
# Additional hack: If the directory does not exist, but the file ${1}.lnk 
# does, the contents of that file will be used an (absolute) path to 
# a directory to be used instead of the specified one.  This is to 
# support splitting out configuration files proprietary to a given company 
# into a location from which they can be managed with appropriate 
# confidentiality, and still be supported on systems (e.g. cygwin) that
# do not support symbolic links.  This resolution will only be done once.
init_from_recurse () 
{
    if [ $# -ne 1 ]; then
        echo "init_from called with more than one arg: $*" 1>&2;
	return 1;
    fi
    
    local file_root=$1;

    if [ ! -e ${file_root} -a -e ${file_root}.lnk ]; then
        local new_file_root=`cat ${file_root}.lnk`
	if [ `echo $new_file_root | sed 's/^\(.\).*$/\1/'` != '/' ]; then
	    new_file_root=`dirname $file_root`/$new_file_root;
	fi
	file_root=$new_file_root
    fi
    
     if [ -e ${file_root} ]; then
        init_from ${file_root}	# Do the actual work in this directory

	# Note: We normally we expect archs to be in <os>/<arch> and
	# hostnames to be in <domainname>/<hostname> but this is simpler
	# to code and won't cause us grief unless an archname == an osname
	# or a hostname == a domain name.
	init_from_recurse ${file_root}/OS/$config_os
	init_from_recurse ${file_root}/$config_arch

	local tmp_domainname=$config_domain
	while [ X"$tmp_domainname" != X"" ] ; do
	    init_from_recurse $file_root/$tmp_domainname
	    init_from_recurse $file_root/$config_host

	    tmp_domainname=`echo $tmp_domainname | sed 's/^[^.]*\.*//'`
	done
    fi
}

# A little sad to make this a basic tool, but it allows me to have 
# files that can be read by either bourne or C shell variants.
function setenv ()
{
    export $1=$2;
}
