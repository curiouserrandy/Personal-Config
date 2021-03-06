suffix_path_with_dirlist PATH /usr/local/git/bin
suffix_path_with_dirlist MANPATH /usr/local/git/man /usr/local/git/share/man
export GIT_PAGER=""

unset -f git

git_binary_location=`type -p git`

git_branch () {
    if $git_binary_location branch > /dev/null 2>&1 ; then
        br=`$git_binary_location branch 2>&1 | sed -n '/^\*/s/^..//p'`
        echo " [$br]"
    fi
}

if ! echo $shell_prompt_commands | tr ' ' '\012' | grep git_branch > /dev/null; then
    shell_prompt_commands="$shell_prompt_commands git_branch"
fi

git() {
    $git_binary_location "$@"
    result=$?
    reset_shell_prompt
    return $result
}
