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

function init_from ()
{
    if [ $# -ne 1 ]; then
        echo "incorporate_for_initialization called with wrong number of args: $*" 1>&2;
	return 1;
    fi
    
    local file_root=$1;
    
    # If a file (with .bf), read it.  If a directory, read
    # <dir>/all.bf if exists, and put <dir>/Bin on path if it exists.
    if [ -r ${file_root}.bf ]; then
	if [ "$INIT_FROM_LOG" = "yes" ]; then echo "IFing file ${file_root}.bf"; fi
        . ${file_root}.bf;
    elif [ -e ${file_root} ]; then
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
	fi
    else
	if [ "$INIT_FROM_LOG" = "yes" ]; then echo "IFing ${file_root} failed"; fi
    fi
}

# A little sad to make this a basic tool, but it allows me to have 
# files that can be read by either bourne or C shell variants.
function setenv ()
{
    export $1=$2;
}
