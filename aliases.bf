# Setenv was done in tools.
function unsetenv () {
    export -n "$@"
}

# Search for files matching the pattern in $1 in all the directories listed
# in the path in $2.  We assume for the moment that there are no spaces 
# in the path in $2.
function searchpath() {
    for i in `echo $2 | tr ':' ' '` ; do
        z=${i}/$1
	if test "$z" = "`eval echo $z`" ; then
	    /bin/true;
	else
	    eval ls $z '|' cat
	fi
    done
}

# Do the above for manpath for all things of the form man*/$1.*
function searchman() {
    searchpath 'man*/'"$1"'.*' $MANPATH
}

# Do the above for executables in the path (i.e. the same as "type")
function searchexe() {
    searchpath $1 $PATH
}

canonicalize_directory () {
    local sd=$PWD;
    cd $1;
    /bin/pwd;
    cd $sd;
}

function tarcopy() {
    # Copy from directory $1 to directory $2, creating $2 if it doesn't
    # exist.
    # One potential argument; -follow means to follow symlinks, copying 
    # the files they point to.
    local follow="";
    case $1 in 
    -follow) follow="h";	# Argument to follow symlinks in tar.
    	     shift;
	     ;;
    *) ;;
    esac
    
    if [ ! -d $1 ]; then
        echo "$1 not a directory" 1>&2 
	return 1;
    fi
    
    if [ -r $2 -a ! -d $2 ]; then
	echo "$2 is not a directory" 1>&2
	return 1;
    fi
    local src=`canonicalize_directory $1`;
    # Must exist before we canonicalize it.
    if [ ! -d $2 ]; then
	if mkdir -p $2 ; then
	    true;
	else
	    return 1;		# mkdir already printed the error message.
        fi
    fi
    local dest=`canonicalize_directory $2`;

    (cd $src ; tar c${follow}f - . | (cd $dest ; tar xBf -))
}

# Re-read .bashrc
rrb () {
    . ~/.bashrc
}

# Tar up configuration files.  Single argument which is file to create; 
# "-" means stdout.
tarconfig () {
    # Relative to $HOME
    local config_files="Config .bashrc .bash_profile .emacs .fvwmrc";
    local cwd=$CWD;
    cd $HOME;
    tar cvf $1 $config_files;
    cd $cdw;
}

# Object file aliases

# Search a library to find out what object file in the library 
# has definitions or references to a given symbol.
# Arguments are:
#	[-prog <nm program>] symbol_name libraries
# The symbol name must not have any funny characters in it
symfind () {
    local symbol_name;
    local libraries;
    local nm_program=nm;
    while [ $# -gt 0 ]; do
        case $1 in
	-prog) 
	    if [ $# -eq 1 ]; then
	        echo "No argument given to -prog" 1>&2;
		return 1;
	    fi
	    nm_program=$2;
	    shift;
	    shift;
	    ;;
	-*)
	    echo "Unrecognized flag $1" 1>&2;
	    return 1;
	    ;;
	*)
	    break;
	    ;;
	esac
    done
    if [ $# -lt 2 ]; then
    	echo "Usage: symfind [-prog <nm program>] symbol library library ..." 1>&2;
	return 1;
    fi
    symbol_name=$1;
    shift;
    libraries="$*";
	
    $nm_program $libraries | \
    	egrep '(\.o|'"$symbol_name"')' | \
	awk '/:/ {last_line=$0; printed=0; next;} { if (!printed) { print last_line; printed = 1; } print $0;}'
}


remacs () {
    rsh -n $1 PATH=/bin:/usr/bin:/usr/local/bin:/usr/X11/bin emacs -font 7x14 -display $DISPLAY &
}

# Give me just the path.
alias which="type -path"

# X windows stuff.
. $config_files_directory/xaliases.bf

