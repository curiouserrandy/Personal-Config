# Allow our environment to override us.  Probably need to come up with
# a better way at some point in the future.
if [ "$GYP_DEFINES" = "" ]; then
    # linux_use_debug_fission=0 is to enable lldb debugging.
    export GYP_DEFINES="component=shared_library disable_nacl=1 python_ver=2.7 clang=1 use_goma=1 linux_use_debug_fission=0 debug_extra_cflags=-fstandalone-debug"
    export DO_NOT_RESTART_PYTHON_FOR_PYAUTO=1
    if [ "`uname -a | grep x86_64`" = "x86_64" ]; then
	export GYP_DEFINES="$GYP_DEFINES target_arch=x64"
    fi
fi

export GYP_GENERATORS=ninja

export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox
