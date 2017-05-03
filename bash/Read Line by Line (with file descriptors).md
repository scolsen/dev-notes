While a basic while loop is often fine for reading files line by line, we can do process faster using file descriptors and slightly obtuse syntax:

```sh
>$OUPUT # Clears contents of output file.
exec 4<&1 # set stdout to redirect to file descriptor 4
exec 1> $OUTPUT # Set the target to output
while read LINE
do
  echo $LINE
done < $INPUT
exec 1<&4 # unassociate stdout from file descriptor
exec 4>&- # close file descriptor.
```

Reference: Mastering Unix Shell Scripting