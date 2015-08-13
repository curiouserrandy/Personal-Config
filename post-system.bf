# Functions for setting prompts in an interactive shell.
if [ "`whoami`" = "root" ]; then
    pchar='#';
else
    pchar='$';
fi

# Just show the last two elements of the directory path in the prompt.
# If you are under the home directory, show it as "~" and count it as 
# one element.

# Data driven based off of "shell_prompt_commands".  These are a list of
# (single word) commands that are executed in order and their output
# concatenated to make the shell prompt.

pwd_at_last_prompt=

last_two_elements_of_pwd () {
    hd=~
    if [ X"$pwd_at_last_prompt" != X"$PWD" ]; then 
      echo $PWD | sed 's;^'"$hd"';~;' | sed 's;^..*/\([^/]*/[^/]*\);../\1;'
    fi
    pwd_at_last_prompt=$PWD;
}

short_host () {
    echo $config_host:
}
	
shell_prompt_commands="short_host last_two_elements_of_pwd $shell_prompt_commands"

reset_shell_prompt () {
    res=""
    for f in $shell_prompt_commands; do
        res="${res}`$f`"
    done
    PS1="$res $pchar "
}

export PROMPT_COMMAND=reset_shell_prompt

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

# Read in my aliases
. $config_files_directory/aliases.bf

# Attempt to get non-interactive shells to pull in aliases also
export ENV=$HOME/.bashrc
export BASH_ENV=$HOME/.bashrc
