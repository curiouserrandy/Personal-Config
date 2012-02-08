export GYP_GENERATORS=make

if [ "$GYP_DEFINES" = "" ]; then
    #export GYP_DEFINES=clang=1		# < Lion, I believe redundant.
    export GYP_DEFINES=mac_sdk=10.6
fi
