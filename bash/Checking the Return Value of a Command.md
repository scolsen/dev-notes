We can check the return value of an executed command using a simple if statement:

```sh
if [ -d /usr/local/bin ]; then 
  echo 'directory exists' # -d returned 0
else
  echo 'directory does not exist' # -d did not return 0
fi
```

We can also access the return value of a command using `$?`, but this is typically not necessary and requires more code. In certain cases it may be needed:

```sh
test -d /usr/local/bin
if [ "$?" -eq 0 ]; then
  echo 'directory exists'
else
  echo 'directory does not exist'
fi
```

