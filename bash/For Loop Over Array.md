A basic for loop over array values in bash.

```sh
ARR=('foo', 'bar', 'bah')
for i in "${ARR[@]}"
do
  echo "$i" #echoes ARR value
done
```

The somewhat odd `​“${ARR[@]}”`​ syntax is important as it indicates the entire array, whereas just $ARR may only hook into one value in the array.

