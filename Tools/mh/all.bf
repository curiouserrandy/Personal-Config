mharch () {
    # Setup the help and usage messages
    tf=/tmp/$$.helpfile
    cat > $tf <<EOF
$0 is a function for archiving mh messages into Berkeley style mail files
under the directory ~/ArchiveMail.  The first non-flag argument must be
a filename (relative to ArchiveMail) into which to archive messages; 
the rest of the arguments are an mh style message list specification.
The flag arguments taken are:
	-folder <folder>	Specify the mh folder to archive from
				(optional; default is current folder).
				No "+" should be placed on the folder name.
	-k|-keep		Don't delete the mh messages from the current 
				folder .
EOF
    helpString="`cat $tf`" ;
    rm -f $tf;
    usageString="$0 [-folder <folder>] <archive file> <message list>";

    # Parse all arguments.
    local usefolder=no;
    local dontdelete=no;
    while [ $# -ne 0 ]; do
	case $1 in 
	-k|-keep)
	    dontdelete=yes;
	    ;;
	-folder) 
	    if [ $# -lt 2 ]; then
            	echo "Usage: $usageString" 1>&2;
	    	return 1;
	    fi
	    usefolder=yes;
	    folder=$2;
	    shift;
	    ;;
	-h|-\?)
	    echo "$helpString" 1>&2;
	    return 1;
	    ;;
	--) shift;		# Everything after this point is taken as
	    break;		# regular args.
	    ;;
	-*)
	    echo "Usage: $usageString" 1>&2;
	    return 1;
	    ;;
	*)  break;
	    ;;
	esac
	shift;
    done

    # Check non-flag arguments.
    if [ $# -lt 2 ]; then
	echo "Usage: $usageString" 1>&2;
	return 1;
    fi

    local archivefile="$1"; 
    shift;

    # We don't put the message list into a variable because we want to be
    # able to expand it out into separate atoms.  We can do this with $@ if
    # we leave it int he positional parameters, but we can't easily if we
    # take it out.
    
    local fullfolder="";
    if [ "$usefolder" = "yes" ]; then
        fullfolder="-folder +$folder";
    fi

    if packf $fullfolder -file "$HOME/ArchiveMail/$archivefile" "$@"; then
	if [ "$dontdelete" = "no" ]; then
            rmm $fullfolder "$@";
	fi
    fi
}
    

# List first set of messages from default0
mhlist0 () {
    scan +default0 first:20
}

# List messages from a specific person
# (Should also have an option to list messages to that person ***)
mhpick0 () {
    scan +default0 `pick +default0 --from "$1"`
}

# Archive messages in sequence pseq to the file specified and remove them.
mharch0 () {
    packf +default0 pseq -file $1 
    rmm +default0 pseq
}

# Delete this message and show next one.
rmn () {
    rmm; next;
}

