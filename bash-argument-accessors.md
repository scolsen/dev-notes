# Bash special sequences to access command line args
Bash has several sequences for accessing commandline arguments:
Reference: Mastering Unix Shell Scripting.
* $\* Match *all* command line arguments.
* $@ match *all* command line arguments.
* "$\*" Take the entire list as one argument with spaces between each.
* "$@" separate the entire list into separate aruments.
