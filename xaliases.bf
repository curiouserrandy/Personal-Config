
# Send escape characters to change the title of the current xwindow.  
# -i means just change icon name, -w means just change window name. 
# All the other arguments are taken as the new title.  
# -- means take rest of arguments as the new title (even if they start with
# a -).
xtitle () {
    local ireq=no;
    local wreq=no;
    while [ $# -ne 0 ]; do
        case $1 in 
	-i) ireq=yes;
	    ;;
	-w) wreq=yes;
	    ;;
	--) break;
	    ;;
	-*)
            echo "xtitle: Unknown argument " $1 1>&2;
	    return 1;
	    ;;
	*)  break;
	    ;;
	esac
	shift;
    done

    if [ $# -eq 0 ]; then
        echo "xtitle: No title specified after flag arguments" 1>&2;
	return 1;
    fi
    local title="$*";
    if [ "$ireq" = "no" -a "$wreq" = "no" ]; then
        # Default; do both.
	ps=0;
    elif [ "$ireq" = "yes" -a "$wreq" = "yes" ]; then
    	# They specified the default by hand.
	ps=0;
    elif [ "$ireq" = "yes" -a "$wreq" = "no" ]; then
        ps=1;
    elif [ "$ireq" = "no" -a "$wreq" = "yes" ]; then
        ps=2;
    fi

    # Counting on the bash builtin behavior
    echo -e "\033]$ps;$title\a";
}
    
     
