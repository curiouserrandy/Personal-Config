if [ X"$config_os" = X"Darwin" ]; then
    sernum=`system_profiler SPHardwareDataType | grep -i "serial number" | head -1 | sed 's/^.*: //'`;
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
	W80141BG66H|W80201SXAGY|C02FH04VDNKY|C02FK00KDNKY|C02FW0GHDNKY)
	    # We're on (one of) my google supplied laptop(s)
	    config_host=rdsmith-macbookpro;
	    config_domain=cam.corp.google.com;
	    ;;
        H00180H620H)
	    # Mac desktop at Google
	    config_host=rdsmith-macpro;
	    config_domain=cam.corp.google.com;
	    ;;
    esac
fi
