# Functions

The `setf` function clobbers the prior value of whatever variable it writes to:

```lisp
(setf *var* (read in))
```

To pass functions as arguments in lisp we need to prefix them with `#'`, otherwise, lisp will treat the function name as a regular variable name.

```lisp
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9))
;; returns (2 4 6 8)
```

You can write anonymous functions in lisp using `lambda`

```lisp
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9))
;; Anon func: (lambda (x) (= 0 (mod x 2)))
```


