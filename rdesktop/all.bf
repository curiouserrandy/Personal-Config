umask 002	# For sharing in crosby with Lori.

prefix_path_with_dirlist PATH /usr/X11R6/bin

#alias getmail="echo No remote mail until new computer"
alias getmail="fetchmail; date"
#alias getkmail="popclient -k -u randy5 -c pop.ne.mediaone.net | formail -s procmail"

alias worldmail="finger randy@world.std.com | tr -d '\015' | awk '/^randy[ 	]/,/new mail/'"

export MOZILLA_HOME=/usr/local/netscape
export MOZILLA_SHARED_REGISTRY=1

PROCMAILDIR=/var/spool/mail/randy-procmail

function snfrom () {
    from $PROCMAILDIR/$1
}

function snsenders () {
    senders $PROCMAILDIR/$1
}

function snsubs () {
    subjects $PROCMAILDIR/$1
}

function snwaiting () {
    ls -algt $PROCMAILDIR | head -23
}

function snmail () {
    Mail -f $PROCMAILDIR/$1
}

function snwatch () {
    tail -f $PROCMAILDIR/Logs/splitlog | awk -f ~/bin/snarfed.awk
}

function waitppp () {
    while ! netstat -i | grep ppp ; do
	sleep 4;
    done
}

function watchmess () {
    tail -f /var/log/messages
}

function rlw() {
    rlogin -8 world.std.com
}

function world () {
    ssh world.std.com
}    

# Other bash packages to pull in.
init_from $config_files_directory/Tools/mh
init_from $config_files_directory/Tools/perl
init_from $config_files_directory/Tools/perforce
init_from $config_files_directory/Tools/sn
