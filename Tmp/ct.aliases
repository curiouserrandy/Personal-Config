# Alias name musings
# Just fine (by repetitive use):
#	ct
#	ctco
#	ctuco
#	ctci
#	ctcom
# Just fine (easy to remember):
#	ctfreeze
#	ctthaw
#	ctdiff
# Ok but not great (name easy to remember, but what does it do?)
#	ctmerge
#	cttrack
# Ok but not great (by repetitive use):
#	ctlaco
#	ctlasco
# Bad (actively don't use)
#	ctlcom
#	ctlscom
# The rest are questionable and should be thought about:
#	ctlvp
#	ctlco
#	ctlsco
#	ctsr 
#	ctxdiff
#	ctmdiff
#	ctlmreq
#	ctlsbranches
#	ctshowner
# New
#	ctres
#	ctures

# Variables

# Default branch to use for various "private branch" manueverings (checkouts, 
# merges, etc).  May be overriden in a site-specific manner.
ct_default_private_branch=randy_private

# Basic alias
alias ct=cleartool

# Standard checkout (everywhere I've been, at least)
# is unreserved, no comment.
ctco () {
    cleartool checkout -unreserved -nc $*
}

# Uncheckout; make sure to do files first.
# Base function; single argument indicating keep or remove.
ctuco_base () {
    local arg;
    case $1 in
    -keep)
        arg=-keep;
        shift;
	;;
    -rm)  
        arg=-rm;
	shift;
	;;
    *) 
        echo "Bad argument to ctuco_base: $1" 1>&2;
	return 1;
	;;
    esac

    # Do the real work.
    local dirs="";
    local files="";
    for i in $*; do
        if [ -d $i ]; then
	    dirs="$dirs $i";
	else
	    files="$files $i";
	fi
    done

    # Do file uncheckouts first
    if [ "$files" != "" ]; then
        cleartool uncheckout $arg $files;
    fi
    if [ "$dirs" != "" ]; then 
        cleartool uncheckout $arg $dirs;
    fi
}

ctuco () {
    ctuco_base -rm $*;
}

ctukco () {
    ctuco_base -keep $*;
}

# Checkin; -nc because my habit is to add comments as I modify the files.
ctci () {
    cleartool checkin -nc $*
}

# Add comment.  We use -cq, assuming that if we want to specify a comment 
# by -c we can do so and that will override -cq.
ctcom () {
   cleartool chevent -append -cq $*
}

ctlvp () {
   cleartool lsprivate $*
}

ctlaco () {
   cleartool lscheckout -avobs -cview $*
}

ctlasco () {
   cleartool lscheckout -avobs -cview -short $*
}

ctlco () {
   cleartool lscheckout -recurse -cview $*
}

ctlsco () {
   cleartool lscheckout -short -recurse -cview $*
}

ctsr () {
   env | grep CLEARCASE_ROOT
}

ctdiff () {
    local files;
    if [ $# -eq 0 ]; then
        files="`cleartool lscheckout -avobs -cview -short`";
    else
        files="$*";
    fi

    for i in $files; do
   	cleartool diff -serial -pre $i
    done
}

ctxdiff () {
   local fromvers=`cleartool lscheckout -cview $1 | sed 's/^.*from \([^ 	]*\) .*$/\1/'`;
   xcleardiff -b $1@@$fromvers $1
}

# Diff between the predecessor version and the last item on the pdv's branch.
ctmdiff () {
    local arg=$1;
    local pdv=`cleartool describe -fmt "%f\n" $arg`;
    local lbv=`echo $pdv | sed 's;/[0-9][0-9]*$;/LATEST;'`
    echo "Differences in $arg between $pdv and $lbv";
    diff $arg@@$pdv $arg@@$lbv;
}

ctlmreq () {
    local files="`cleartool lscheckout -avobs -cview -short`";

    local i;
    echo "File Version_to_be_merged_from"
    for i in $files; do
        local pdv=`cleartool describe -fmt "%f\n" $i`;
        local lbv=`echo $pdv | sed 's;/[0-9][0-9]*$;/LATEST;'`

        if diff $i@@$pdv $i@@$lbv > /dev/null; then
	    /bin/true;
	else
	    echo "$i $lbv";
	fi
    done
}    

# Arg 1 is file arg 2 is version to merge from.
ctmerge () {
    ct merge -narrows -g -to $1 -version $2
}     


# Bring all checked out files up to date with current version in 
# backing store.
# This uses findmerge; it does not change the predecessor version but 
# does connect a merge arrow from the version moved from to the 
# checked out version (this allows future checkin).
# If not arguments are given, it uses the output of a ctlasco; otherwise
# it updates just the arguments given.
# By default, if the predecessor version is on a branch with my user name
# at the beginning of it, it will merge from the latest version on the 
# parent branch.  If the "-noprivate" flag is given it, it will always 
# merge from the latest version on the precessessor versions branch.
cttrack () {
    local user=`whoami`;
    local files;
    local process_private=y;
    local action="-merge";
    if [ $# -ne 0 ]; then
        case $1 in
	-noprivate)
	    process_private=n;
	    shift;
	    ;;
	-show)
	    action="-print";
	    shift;
	    ;;
	-*)
	    echo "Bad argument " $1 " given to cttrack" 1>&2;
	    return 1;
	    ;;
	*) ;;
	esac
    fi

    if [ $# -eq 0 ]; then
        files=`cleartool lscheckout -avobs -cview -short`;
    else
        files="$*";
    fi
    
    local logfile=/tmp/cttrack.$$;
    rm -f $logfile;
    for i in $files; do
        local pdv=`cleartool describe -fmt "%f\n" $i`;
        local lbv=`echo $pdv | sed 's;/[0-9][0-9]*$;/LATEST;'`;
        if [ "$process_private" = "y" ]; then
	    lbv=`echo $lbv | sed 's;/randy[^/]*/LATEST;/LATEST;'`;
	fi
	
	cleartool findmerge $i -nc -log $logfile.tmp -fversion $lbv $action;
	if [ $? -ne 0 ]; then
	    cat $logfile.tmp >> $logfile;
	fi
	rm -f $logfile.tmp;
    done
    if [ -r $logfile ]; then
        cat $logfile;
    fi
    rm -f $logfile;
}

ctlcom () {
    ct lshistory -nco $*
}

ctxlcom () {
    ct lshistory -g $*
}

# Freeze a view by putting down a timestamp equal to the current time 
# in the config spec.  This goes at the front of the config spec; it won't
# affect checked out files because it only affects the definition of LATEST.
# Optional argument is name of view to freeze.
ctfreeze () {
    local view_name="`cleartool pwv -s`";
    if [ "$1" != "" ]; then
        view_name=$1;
    fi
    cleartool startview $view_name;
    local tmpfile=/tmp/freezetag.$$;
    date | awk '{printf("time %s-%s-%s.%s\n", $3, $2, $6, $4);}' > $tmpfile;
    cleartool catcs -tag $view_name | grep -v '^time' >> $tmpfile;
    cleartool setcs -tag $view_name $tmpfile;
    rm -f $tmpfile;
}

# Thaw a view (remove any timestamp from it).
# Optional argument is name of view.
ctthaw () {
    local view_name="`cleartool pwv -s`";
    if [ "$1" != "" ]; then
        view_name=$1;
    fi
    cleartool startview $view_name;
    local tmpfile=/tmp/thawtag.$$;
    cleartool catcs -tag $view_name | grep -v '^time' > $tmpfile;
    cleartool setcs -tag $view_name $tmpfile;
    rm -f $tmpfile;
}


# List all branch types in clearcase (probably most useful piped into 
# greps of various sorts, i.e. "| grep randy".
# Note that this lists types associated with the vob that the current
# directory is in.
ctlsbranches () {
    cleartool lstype -short -kind brtype 
}

# Show owner (who's touched it most) information for a file or files.
# Takes a list of files; shows information for each.
ctshowner () {
    local i;
    for i in $*; do
    	echo "-- $i";
        cleartool lshistory -fmt "%u\n" $i | \
	    awk '{count[$1]++;} 
	    	 END {for (c in count) {print "\t", count[c], c;}}' | \
 	    sort -nr
    done
}


# Change unreserved checkouts to reserved.  Defaults to the list of
# checked out files in the current view.
ctres () {
    local files;
    if [ $# -eq 0 ]; then
	files="`cleartool lscheckout -avobs -cview -short`";
    else
        files="$*";
    fi
    
    cleartool reserve -nc $files;
}

ctures () {    
    local files;
    if [ $# -eq 0 ]; then
	files="`cleartool lscheckout -avobs -cview -short`";
    else
        files="$*";
    fi
    
    cleartool unreserve -nc $files;
}

# Clearcase doesn't like to invoke emacs as a windowing editor; force it to.
if echo $EDITOR | grep macs > /dev/null ; then
    export WINEDITOR=$EDITOR;
fi

# *** Private branch aliases
# The following aliases are based on a specific pattern of using clearcase, 
# that of having a (or more than one) primary debug views, and then other
# integration test views in which you put the current set of fixes together 
# and do final testing with that set (possibly with debugging turned
# off, or with more build options, or whatever).  The following code
# assumes this model, and sets up a method where you move files
# between the sandboxes on a private branch (default name is set in
# ct_default_private_branch).  This branch is considered temporary; it
# is created on each file when you check out the file in your
# development sandbox, and is deleted just before you check in in your
# integration sandbox.  Note that it is important that it be deleted *just
# before* you check in the integration sandbox, elsewise the file
# modification times in your development sandbox will move backwards,
# with bad consequences for any builds you do between branch deletion and 
# integration check in.
#
# Obviously, your config specs must support this.  In the source
# sandboxes, there must be autobranchs for your private branch (s) and
# recognition of files on that branch; this cannot be true on your
# destination branch.  
# cttrack is also somewhat aware of this pattern, but not dependent on
# it, so it stays above.
#
# The commands are:
#
# -- ctprmerge merges in all files from a particular branch into your
# current sandbox.   
# -- ctprclean removes a particular branch 
# -- ctprls lists all elements that have a particular branch anywhere in
# the vobs.

# The recommended usage is:
# Fiddle in your developement sandbox; check in to your private branch.
# ctprmerge
# <Optional testing>
# ctres # == (ct reserve -nc `ctlasco`)
# ctprclean
# ctci `ctlasco`

# Merge in from my private branch.  Takes a single argument, which is
# the branch name to merge from; this defaults to
# $ct_default_private_branch.  Acts on the current view, merging in
# all files from the branch named.  The comments on the elements will
# be the concatentation of all comments on the branch on that element.
ctprmerge () {
    local branch_name=$ct_default_private_branch;
    case $# in
    0) ;;
    1) branch_name=$1;
       ;;
    *) echo "Too many arguments specified: $*" 1>&2;
       return 1;
       ;;
    esac

    # Do the merge.
    ct findmerge -log /tmp/fm.$$ -avobs -fversion .../$branch_name -merge -unreserved -nc
    
    # Get the files the merge was done on.
    local files=`sed 's/^#\{0,1\}cleartool findmerge \(\/vobs.*\)@@.*$/\1/' < /tmp/fm.$$`;
    rm -f /tmp/fm.$$;

    # Set the comments.
    local i;
    for i in $files; do
        ct lshistory -directory -branch $branch_name -fmt "%c" $i > $i.$$.com
        ct chevent -append -cfile $i.$$.com $i
	echo rm -f $i.$$.com
    done
}

    
# Clean off a branch off of the checked out files within a view.  This 
# command is intended to be used after the above command; the stuff off of
# the (e.g.) randy_private branch has been merged back to the mainline, 
# even if not checked in, and that branch can be deleted.  

# Takes an argument which is the branch name; this name defaults
# to $ct_default_private_branch.
ctprclean () {
    local branch_name=$ct_default_private_branch;
    case $# in
    0) ;;
    1) branch_name=$1;
       ;;
    *) echo "Too many arguments specified: $*" 1>&2;
       return 1;
       ;;
    esac

    for i in `cleartool lscheckout -avobs -cview -short`; do
	if cleartool lsvtree -short -branch .../$branch_name $i 2> /dev/null > /tmp/$$.tmp; then
	    local branch_pname=`head -1 /tmp/$$.tmp`;
            cleartool rmbranch -nc -force $branch_pname;
	else
	    echo "**File $i does not have branch $branch_name";
	fi
	rm -rf /tmp/$$.tmp;
    done
}

# Lists all instances of a particular branch in all the vobs.  Single argument
# is branch name; defaults to $ct_default_private_branch.
ctprls () {
    local branch_name=$ct_default_private_branch;
    case $# in
    0) ;;
    1) branch_name=$1;
       ;;
    *) echo "Too many arguments specified: $*" 1>&2;
       return 1;
       ;;
    esac

    cleartool find -avobs -branch brtype\($branch_name\) -print
}
    
