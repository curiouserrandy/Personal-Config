# Mail related aliases
# --------------------

senders () {
    formail -x From: -s < $1 | \
	sed -e 's/^[ 	]*//' \
	    -e 's/[ 	]*$//' \
	    -e 's/(.*)//' \
	    -e '/</s/^.*<\(.*\)>.*$/\1/' | \
	sort | uniq
}

subjects () {
    formail -x Subject: -s < $1
}

function snfrom () {
    from /usr/spool/mail/randys-pmd/$1
}

function snsenders () {
    senders /usr/spool/mail/randys-pmd/$1
}

function snsubs () {
    subjects /usr/spool/mail/randys-pmd/$1
}

function snwaiting () {
    ls -algt /usr/spool/mail/randys-pmd | head -23
}

function snmail () {
    Mail -f /usr/spool/mail/randys-pmd/$1
}

function snwatch () {
    tail -f /usr/spool/mail/randys-pmd/Logs/splitlog | awk -f ~/bin/snarfed.awk
}

