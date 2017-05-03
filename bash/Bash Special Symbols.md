The following symbols are interpreted as commands with special functionality in shell scripts:

```sh
( )       # runs enclosed command in a sub-shell
(( ))     # Evaluates and assigns a value to a variable            after doing math.
$(( ))    # Evaluates enclosed expression
[ ]       # alias for the test command.
< >       # used for string comparison
$ ( )     # Command substitution
`command` # command substitution.
```

Reference: Mastering Unix Shell Scripting.