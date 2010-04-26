#!/bin/bash

# Synchronize all files on home machine.

if [ ! -d /Volumes/randy ]; then
    echo "Randy volume not mounted" 1>&2;
    exit 1;
fi

if [ ! -d /Volumes/crosby ]; then
    echo "Crosby volume not mounted" 1>&2;
    exit 1;
fi

$config_files_directory/Bin/filesync -t -f - /Volumes/randy ~ << EOF
Personal/CD.txt
Personal/Log
Personal/Relationships
Personal/computer.accounts
Personal/contra.txt
Personal/Recreation
Personal/Gaming
Personal/LifeOrg
Projects
random.info
Whiteboard
Job
EOF

failure_out=$?;

$config_files_directory/Bin/filesync -t -f - /Volumes/randy/tmp/MacBackup ~ << EOF
Documents/iChats
Library/Mail
Config
random.info
StarSaga
EOF

if [ $failure_out -eq 0 ]; then
    failure_out=$?;
fi

$config_files_directory/Bin/filesync -t -f - /Volumes/crosby ~/Crosby << EOF
numbers locations etc
travel info/packing list.doc
travel info/packing lists
EOF

if [ $failure_out -eq 0 ]; then
    failure_out=$?;
fi

$config_files_directory/Bin/filesync -t -f - /Volumes/crosby ~/House << EOF
Taxes
EOF

if [ $failure_out -eq 0 ]; then
    failure_out=$?;
fi

# This grabs too much stuff, and probably stuff that changes.  
# Reduce it down (maybe just to mailboxes).
# $config_files_directory/Bin/filesync -t -f - -X Caches /Volumes/randy/MacBackup ~ << EOF
# Library
# EOF

if [ $failure_out -eq 0 ]; then
    failure_out=$?;
fi

exit $failure_out


