prefix_path_with_dirlist PATH /usr/X11/bin
suffix_path_with_dirlist PATH /usr/local/jdk1.2/bin

#alias getmail="echo No remote mail until new computer"
alias getmail="popclient -u randy5 -c pop.ne.mediaone.net | formail -s procmail"
alias getkmail="popclient -k -u randy5 -c pop.ne.mediaone.net | formail -s procmail"
# export PATH=$PATH:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/jdk/bin
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/lib:/usr/lib:/usr/local/jdk/lib

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
    rlogin -8 world.std.com
}    

# Other bash packages to pull in.
init_from $config_files_directory/Tools/mh
