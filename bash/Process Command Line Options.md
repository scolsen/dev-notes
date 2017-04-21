This is a common idiom for processing command line options on invocation of a shell script. We use `​`​the `​test`​ command to evaluate whether the argument count `​$#` is greater than 0.

```sh
while test $# -gt 0
do 
  case $1 in
    --option | --o | --otherOptionAlias )
      myFunction
      myvariable=1
      ;;
    --help | --h | --etc )
      helpFunction
      exit 0
      ;;
    -* )
      error "Unrecognized option: $1"
      ;;
    * )
    break 
    ;;
  esac
  shift 
done 
```

The options provided in this code are samples. The case blocks provide some common execution patterns when an option is matched; run some function, set some variable value, or run some function and exit. The `​-*`​ command matches any unrecognized options and throws an error. The last case matches any output that is not a recognized option and breaks.

We shift after running the case to move the next argument up in the list. The shift is important, as it also ensures our condition for the while statement eventually hits 0.

Courtesy of *Classic Shell Scripting - *Arnold Robbins and Nelson Beebe