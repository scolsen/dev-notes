A simple way to add an element to an array in bash, equivalent of push.

```sh
ARR=()
ARR=("${ARR[@]}" "element")
```

Above, we create an empty array, then reassign it’s value to the preexisting value and whatever element we want in addition. Repeating this pattern, we can incrementally sdd elements to an array.