if [ X"$config_os" = X"Darwin" ]; then
    sernum=`system_profiler SPHardwareDataType | grep -i "serial number" | sed 's/^.*: //'`;
    case $sernum in
        W851305TSQ5)
            # We're on blueboat, my personal laptop.
	    # The domain is user dependent.
	    config_host=blueboat
	    if [ X"$USER" = X"rsmith" ]; then
	        config_domain=nane.netapp.com;
	    else
	        config_domain=tigana.org
	    fi
	    ;;
	W80141BG66H)
	    # We're on my google supplied laptop
	    config_host=rdsmith-macbookpro;
	    config_domain=cam.corp.google.com;
	    ;;
    esac
fi
