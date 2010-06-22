suffix_path_with_dirlist PATH /usr/local/git/bin
suffix_path_with_dirlist MANPATH /usr/local/git/man /usr/local/git/share/man
export GIT_PAGER=""

git_branch () {
    if git branch > /dev/null 2>&1 ; then
        br=`git branch 2>&1 | awk '$1 == "*" {print $2;}'`
        echo " [$br]"
    fi
}

shell_prompt_commands="$shell_prompt_commands git_branch"

