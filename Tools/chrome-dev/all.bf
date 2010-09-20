suffix_path_with_dirlist PATH ~/Sandboxen/depot_tools
if [ "`uname`" = "Linux" -a "`uname -a | grep x86_64`" = "x86_64" ]; then
    export GYP_DEFINES=target_arch=x64
    export GYP_GENERATORS=make
fi

suffix_path_with_dirlist PATH ~/Sandboxen/depot-tools

# Location of patches repository
if [ "$config_os" = "cygwin" ]; then
    chrome_patches=//filer/home/rdsmith/Repositories/ChromePatches.git
elif [ "$config_host" = "rdsmith-macbookpro" ]; then
    chrome_patches=ssh://astibar.cam.corp.google.com/~rdsmith/Repositories/ChromePatches.git
else
    chrome_patches=/home/rdsmith/Repositories/ChromePatches.git
fi

# Email for trybots (git try doesn't quite do the same thing as git for email.)
export TRYBOT_RESULTS_EMAIL_ADDRESS=$EMAIL

