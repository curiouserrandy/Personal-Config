# .bash_profile

# Get the aliases and functions if we're interactive
load_files=no
if [ "$force_file_load" = "yes" ]; then
    load_files=yes;
fi

case $- in
    *i*)load_files=yes;
        ;;
esac

if [ "$load_files" = "yes" ]; then
    if [ -f ~/.bashrc ]; then
	. ~/.bashrc
	PATH=$PATH:$HOME/bin
	BASH_ENV=$HOME/.bashrc
	USERNAME=""
	export USERNAME BASH_ENV PATH
    fi
    ;;
fi
