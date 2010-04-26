export OPENWINHOME=/usr/openwin

# Sun has ucb stuff in /usr/ucb; let it override default SysV behavior.
prefix_path_with_dirlist PATH /usr/ucb /usr/openwin/bin /usr/ccs/bin
prefix_path_with_dirlist PATH /opt/SUNWspro/bin 
suffix_path_with_dirlist MANPATH ${OPENWINHOME}/man
# prefix_path_with_dirlist LD_LIBRARY_PATH /usr/ccs/lib /usr/ucblib 


# Really should add option to search manpath 
printman () {
    troff -man $1 | /usr/lib/lp/postscript/dpost | lpr
}
