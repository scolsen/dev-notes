We can produce a random number in a bash script as follows:

```sh
RANDOM=$$ # intialize the value to the PID of the script.
UPPERLIMIT=$1 # user set upper limit.
RANDOM_NUM=$(($RANDOM % $UPPERLIMIT + 1)) # combine to get a random value.
```

If the user passes 100 as the argument, then this produces a random number from 1 to 100.

Reference: Mastering Unix Shell Scripting.