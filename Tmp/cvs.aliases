# Only good for directories under current
cvslvp () {
    cvs -nq update $* | grep '^\?'
}

cvslco () {
    cvs -nq update $* | egrep -v '^(U|\?)'
}

cvsup () {
    cvs -q update -d $*
}

cvsdiff () {
    cvs -q diff -c $*
}
