suffix_path_with_dirlist PATH /usr/local/git/bin
suffix_path_with_dirlist MANPATH /usr/local/git/man /usr/local/git/share/man
export GIT_PAGER=""

unset -f git

git_branch () {
    if git branch > /dev/null 2>&1 ; then
        br=`git branch 2>&1 | sed -n '/^\*/s/^..//p'`
        echo " [$br]"
    fi
}

last_heads_ref=
last_git_pwd=

git_branch_for_prompt () {
    if [ X"$last_git_pwd" != "$PWD" ]; then
	last_heads_ref=
	last_git_pwd=$PWD    
	while true; do
	    if [ -d ./.git ]; then
	        last_heads_ref=$PWD/.git/HEAD
		break;
	    fi
	    
	    if [ X"$PWD" == X"/" ]; then
	        break;
	    fi
	    cd ..
	done
	cd "$last_git_pwd"
    fi
    if [ X"$last_heads_ref" != X"" ]; then
        echo -n " [";
	sed 's;ref:[ 	    ]*refs/heads/;;' "$last_heads_ref" | tr -d '\012'
	echo -n "] ";
    fi
}

suffix_val_to_var_if_not_present git_branch_for_prompt shell_prompt_commands
		
