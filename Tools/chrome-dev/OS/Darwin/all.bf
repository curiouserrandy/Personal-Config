export GYP_GENERATORS=ninja

if [ "$GYP_DEFINES" = "" ]; then
    export GYP_DEFINES=mac_sdk=10.6
fi
