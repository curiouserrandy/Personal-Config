# Load in netapp stuff if I'm connected to NetApp

if ifconfig | grep 'inet ' | grep -v "127.0.0.1" > /dev/null; then
    if host web.nane.netapp.com | grep NXDOMAIN > /dev/null; then
	true;
    else
	init_from $configuration_files_directory/nane.netapp.com
	init_from $configuration_files_directory/nane.netapp.com/OS/$systype
	init_from $configuration_files_directory/nane.netapp.com/OS/$systype/$archtype
	init_from $configuration_files_directory/nane.netapp.com/$force_host
    fi
fi
    
