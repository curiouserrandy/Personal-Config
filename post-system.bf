# Functions for setting prompts in an interactive shell.
if [ "`whoami`" = "root" ]; then
    pchar='#';
else
    pchar='$';
fi

# Just show the last two elements of the directory path in the prompt.
# If you are under the home directory, show it as "~" and count it as 
# one element.
reset_shell_prompt () {
  hd=~
  end_pwd=`echo $PWD | sed 's;^'"$hd"';~;' | sed 's;^..*/\([^/]*/[^/]*\);../\1;'`
  PS1="\h:$end_pwd $pchar "
}

# Replace an element of the path with a different element.
# Useful when you're working in parallel trees.
function massage_path () {
  massage_path_result=`echo $PWD/ | sed 's;/'"$1"'/;/'"$2"'/;'`
}

# Arguments are a list of variables.  This function writes to 
# stdout a list of commands that may be used to replicate the
# status of each variable.
# Possible stati:
#	unset		Variable does not exist as a shell or environment var.
# 	shell not env
#	environmental var.
envdefs () {
    for i in $* ; do
        local pset=`eval 'echo ${'"$i"':+xset}'`;
	local eset=`env | grep -- '^'"$i"'='`;
	local val=`eval 'echo $'"$i"`;
	
	if [ "$eset" != "" ]; then
	    echo "export $i=\"$val\"";
	elif [ "$pset" != "" ]; then
	    echo "$i=\"$val\"";
	else
	    echo "unset $i";
	fi
    done
}

# Read in any bash functions that we've saved in passing.
read_if_exists ~/.bash_save

# Save a list of functions.
save_function () {
    for i in $*; do
        declare -f $i >> ~/.bash_save
    done
}

# Save environ variables
save_environ () {
    envdefs $* >> ~/.bash_save
}

# Actual prompt fiddling; only do if the shell is interactive.

if [ "$PS1" != "" ]; then
    function cd () {
      if test $# -eq 2 ; then
        massage_path $1 $2;
        cd $massage_path_result;
      else
        builtin cd "$@";
        reset_shell_prompt
      fi
    }
    
    function pushd () {
      builtin pushd $*
      reset_shell_prompt
    }
    
    function popd () {
      builtin popd $*
      reset_shell_prompt
    }
    
    reset_shell_prompt
    
    # Terminal setup.
    if [ "$TERM" = "dialup" -o "$TERM" = "network" ]; then
        # This seems to work for my usual.
        eval `tset -sQ ?vt100`
    fi
    
    # Set terminal characteristics correctly
    stty intr '^C' erase '^?'
fi
    
# Read in my aliases
. $config_files_directory/aliases.bf

# Attempt to get non-interactive shells to pull in aliases also
export ENV=$HOME/.bashrc
export BASH_ENV=$HOME/.bashrc
