Bash idiom for reading each line in a file. The -r option prevents backslash escapes from being interpreted.

```sh
while read -r line
do
  echo "$line" #or some other process
done < "myfile.txt"
```

Setting IFS equal to a blank space will prevent trimming of lead/trail whitespace.

```sh
#IFS prevents lead/trail whitespace trimming.
while IFS= read -r line 
do 
  echo "$line" #or some other process
done < "myfile.txt"
```