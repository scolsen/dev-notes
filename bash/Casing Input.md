We sometimes need to ensure input assigned to variables is of a consistent casing. Use `tr` or `typeset` to set the case of variable contents:

```sh
# Set a variable to uppercase using tr
VAR=$(echo 'value' | tr '[a-z]' '[A-Z]')
# Set a variable to lowercase using tr
VAR=$(echo 'value' | tr '[A-Z]' '[a-z]')
# As this example hints, you can utilize tr for a variety of interesting substitutions.

# Set a variable to uppercase using typeset.
typeset -u VAR
VAR='hello' 
echo $VAR # outputs HELLO
# Set a variable to lowercase using typeset
typeset -l VAR
VAR='HELLO'
echo $VAR # outputs hello
```

Source: Mastering Unix Shell Scripting