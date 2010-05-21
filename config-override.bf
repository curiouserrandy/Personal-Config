if [ X"$systype" = X"Darwin" ]; then
    sernum=`system_profiler SPHardwareDataType | grep -i "serial number" | sed 's/^.*: //'`;
    case $sernum in
        W851305TSQ5)
            # We're on blueboat, my personal laptop.
	    # The domain is user dependent.
	    hostname=blueboat
	    if [ X"$USER" = X"rsmith" ]; then
	        domainname=nane.netapp.com;
	    else
	        domainname=tigana.org
	    fi
	    ;;
	W80141BG66H)
	    # We're on my google supplied laptop
	    hostname=rdsmith-macbookpro;
	    domainname=cam.corp.google.com;
	    ;;
    esac
fi
