export GYP_DEFINES="component=shared_library disable_nacl=1 python_ver=2.6"
export GYP_GENERATORS=make,ninja

if [ "`uname -a | grep x86_64`" = "x86_64" ]; then
    export GYP_DEFINES="$GYP_DEFINES target_arch=x64"
fi


