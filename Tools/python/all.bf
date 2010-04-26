## Need to prefix as we need to dodge the installed python in /usr/bin
prefix_path_with_dirlist PATH /Library/Frameworks/Python.framework/Versions/2.6/bin
prefix_path_with_dirlist MANPATH /Library/Frameworks/Python.framework/Versions/2.6/share/man
prefix_path_with_dirlist PYTHONPATH ~/utils/python
export PYTHONPATH
