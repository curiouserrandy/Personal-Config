if [ X"$systype" = X"Darwin" ]; then
    sernum=`system_profiler SPHardwareDataType | grep -i "serial number" | awk '{print $3;}'`;
    if [ X"$sernum" = X"W851305TSQ5" ]; then
        # We're on blueboat, my personal laptop.  The domain is user dependent.
	hostname=blueboat
	if [ X"$USER" = X"rsmith" ]; then
	    domainname=nane.netapp.com;
	else
	    domainname=tigana.org
	fi
    fi
fi
