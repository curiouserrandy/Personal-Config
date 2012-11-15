# Allow our environment to override us.  Probably need to come up with
# a better way at some point in the future.
if [ "$GYP_DEFINES" = "" ]; then
    export GYP_DEFINES="component=shared_library disable_nacl=1 python_ver=2.7 clang=1"
    export DO_NOT_RESTART_PYTHON_FOR_PYAUTO=1
    if [ "`uname -a | grep x86_64`" = "x86_64" ]; then
	export GYP_DEFINES="$GYP_DEFINES target_arch=x64"
    fi
fi

export GYP_GENERATORS=ninja

