Bash has several sequences for accessing commandline arguments:

```sh
$* # Match all command line arguments.
$@ # Match all command line arguments.
"$*" # Take the entire list as one argument with spaces between each.
"$@" # separate the entire list into separate aruments.
```

Reference: Mastering Unix Shell Scripting.