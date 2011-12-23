prefix_path_with_dirlist PATH ~/Sandboxen/depot_tools	# There's a gcl in Google symlinks I don't want.

# Location of patches repository
if [ "$config_os" = "CYGWIN" ]; then
    chrome_patches=//filer/home/rdsmith/Repositories/ChromePatches.git
elif [ "$config_host" = "rdsmith-macbookpro" ]; then
    chrome_patches=ssh://astibar.cam.corp.google.com/~rdsmith/Repositories/ChromePatches.git
else
    chrome_patches=/home/rdsmith/Repositories/ChromePatches.git
fi

# Useful shell variables
if [ "$config_os" = "CYGWIN" ]; then
    devenv='/cygdrive/c/Program Files (x86)/Microsoft Visual Studio 9.0/Common7/IDE/devenv.com'
fi

# Email for trybots (git try doesn't quite do the same thing as git for email.)
export TRYBOT_RESULTS_EMAIL_ADDRESS=$EMAIL

unset -f trunklkgr
trunklkgr_binary_location=`type -p trunklkgr`
trunklkgr () {
    $trunklkgr_binary_location "$@"
    reset_shell_prompt
}

# Add a define to GYP_DEFINES if it's not already there.
add_gyp () {
  if echo $GYP_DEFINES | tr ' ' '\n' | fgrep -w "$1" > /dev/null ; then
      echo \"$1\" already present in GYP_DEFINES: $GYP_DEFINES
  else
      export GYP_DEFINES="$GYP_DEFINES $1"
      echo \"$1\" added to GYP_DEFINES: $GYP_DEFINES
  fi
}

# Setup build to use my plugin.
if [ "$config_os" = "Linux" ]; then
  add_plugin() {
    RANDY_DB_PLUGIN=/usr/local/google/home/rdsmith/Sandboxen/3chrome-newgit/src/third_party/llvm-build/Release+Asserts/lib/libOutputDB.so
    DB_OUTPUT_FILE=/usr/local/google/home/rdsmith/Sandboxen/3chrome-newgit/src/clang-db
    add_gyp clang=1
    add_gyp clang_load=$RANDY_DB_PLUGIN
    add_gyp clang_add_plugin='"complete -Xclang -plugin-arg-complete -Xclang --db='$DB_OUTPUT_FILE'"'
  }
fi
