# Functions

Define functions using `defun`
```lisp
(defun name (parameter*)
  "Optional documentation string."
  body-form*)
```
Unless the return-from special operator is used, the value of the last body form of the function is returned by the function.

By default, parameters are required:
```lisp
;; x is a required arg in the following
(defun hello-world (x) (format t "hello, ~a" x))
```
use &optional to define optional parameters for a function:
```lisp
(defun foo (a b &optional c d) (list a b c d))
```
If optional param vals are not provided they will be passed NIL.

You can provide a default value other than nil for optional params using a list:
```lisp
(defun foo (a b &optional (c 10) (d 4)) (list a b c d))
```
If no c or d argument is provided in the prior case the function will bind those values to 10 and 4 respectively.

Default optional values can also refer to parameters that precede them:
```lisp
(defun make-rectangle (width &optional (height width)) ...)
```

This rectangle function will just return a square if no height arg is provided.

Provide a third value in an optional arg to bind it to a truth value that represents whether or not the arg was provided when called:
```lisp 
(defun foo (a b &optional (c 3 c-supplied-p)) (list a b c c-supplied-p))
```
The prior examples return NIL for c-supplied-p if the arg was not provided, and T if it was.

If a function can take a variadic number of arguments, use &rest to bind all the passed arguments into a named list. All LISP implementations must enable functions to support a minimum of 50 parameters.
```lisp
(defun format (stream string &rest values)...)
```

You can use keyword arguments to remove the positionality requirement of arguments:
```lisp
(defun foo (&key a b c) (list a b c))
(foo :a 1 :b 2) -> (1 2 NIL)
```
When a keyword arg is not provided it works like an optional and uses a default value.
Note that &key must follow any required, &optional or &rest parameters.

You can also make the keyword names and parameter names of the function distinct, as in swift, by using a list instead of a keyword:
```lisp
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p)) 
  (list a b c c-supplied-p)
(foo :apple 10 :box 20 :charlie 30) -> (10 20 30 T)
```

It is inadvisable to mix &optional and &key parameters, as omitted optional params will instead bind the param names to the values intended for the keyword parameters. :w

If you combine &rest and &key the provided args are bound to both the rest list and each key:
```lisp
(defun foo (&rest rest &key a b c) (list rest a b c))
(foo a: 1 b: 2 c: 3) -< ((A: 1 B: 2 c: 3) 1 2 3)


```

Use return-from to explicitly return from a particular point in the function. Return-from takes the block to return from, in this case the function, as a first argument:

```lisp
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n )
        (return-from foo (list i j))))))
```

Lisp supports higher-order functions. Use the function special operator to get a function by name. Function returns the actual function object.
The #' operator is syntactic sugar for function.

After getting a function with function or #', you must call it using funcall or apply. Use funcall when you know the explicit number of arguments you are going to pass to the function at runtime. The first arg is the function to invoke, followed by the number of args to pass to it:
```lisp
(funcall #'foo 1 2 3) ;invoke foo
```
Note that when using a variable to store an unknown function as an arg at runtime, you should not precede the variable with the function operator, as lisp needs to resolve the variable:
```lisp
(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))
```

Apply is the same as funcall, except it expects a list of arguments of arbitrary length instead of individually named arguments. It will apply the function to the values in the list. Apply also allows for explicit arguments preceding the list of args, so long as the last argument is the list:
```lisp
(apply #'plot #'exp plot-data)
```

You can create anonymous functions using lambda:
```lisp
(lambda (parameters) body)

(funcall #'(lambda (x y) (+ x y)) 2 3) -> 5
```
You can also omit the #' function operator when using a lambda as an argument to a function: (funcall (lambda (x y) (x + y)) 2 3) ; works returns 5

