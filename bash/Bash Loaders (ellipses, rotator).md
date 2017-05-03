Techinque for displaying progress indicators in bash scripts. Below is an example of a basic loader function which takes an input value as an argument and continually displays a sequence of elipses.

```sh
dot_loader(){
  echo -ne "$1 . \r"
  sleep 0.2
  echo -ne "$1 .. \r"
  sleep 0.2 
  echo -ne "$1 ... \r"
  sleep 0.2
  echo -ne "$1    \r"
  sleep 0.3
}
```

A rotating slash character also makes for a fine load indicator. The following is an implementation:

```sh
rotate_loader(){
  INTERVAL=0.1 # sleep time.
  COUNT="0" # determines which slash state to render / - | \
  while : # infinite loop until finction is killed.
  do
    COUNT=(( COUNT +  1 ))
    case $COUNT in
      1) echo -e '-'"\b\c"
        sleep $INTERVAL
        ;;
      2) echo -e '\\'"\b\c"
        sleep $INTERVAL
        ;;
      3) echo -e "|\b\c"
        sleep $INTERVAL
        ;;
      4) echo -e "/\b\c"
        sleep $INTERVAL
        ;;
      *) COUNT="0" ;; # reset count
    esac
  done
}
```

To sync these loaders with a running process, we can use the following PID refernce techique:

```sh
loader & # run a loader in the background
LASTPID=$! # get the process ID of the last run command, the loader.
some_long_process # let our process run.
kill -9 $LASTPID # kills the process with ID captured by LASTPID, in this case the loader.
echo "\b\b " # clears the line left over by the loader. 
```

Of course we can render whatever style loader we want, characters that have a clear transition of state are best.