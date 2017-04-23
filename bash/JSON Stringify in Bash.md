This sed one liner will, given an input that is formatted as \<property\>:\<value\>, add necessary quotes and commas to format that input as a valid JSON entry.

```sh
sed -E 's/^(.*)/"\1/; s/:/":"/; s/(.*)$/\1",/;'
```

Notice this sed assumes the input is only a property of the JSON object, e.g. author:Faulkner and not the entirety of the JSON object.

Adding properties sequentially to a simple, flat JSON object is then simple. Simply assign the result of sed to a variable, and concatenate as follows.

```sh
JSON="{$mysedstring"
#rinse and repeat for each property 
JSON="$JSON}" #and close! All done.
```

Of course, we must remove the comma after the last property in our JSON object. We can achieve that with the following sed replacement.

```sh
sed 's/,}/}/'
```

That’s it. After all is said and *run*, we should have a simple JSON object containing quoted and comma delimited properties.