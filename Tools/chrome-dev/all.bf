suffix_path_with_dirlist PATH ~/Sandboxen/depot_tools
if [ "`uname`" = "Linux" -a "`uname -a | grep x86_64`" = "x86_64" ]; then
    export GYP_DEFINES=target_arch=x64
    export GYP_GENERATORS=make
fi

suffix_path_with_dirlist PATH ~/Sandboxen/depot-tools
