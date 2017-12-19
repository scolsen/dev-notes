# macros

Lisp macros have knowledge or the language structure and instruct the compiler to return lisp statements to be evaluated at runtime. You can use macros to generate anonymous functions and remove code duplication. 

To define a macro, use defmacro

It’s important to realize macros operate on the actual expression passed to them and are processed at compile time, and are ‘meta’ by default, for instance the following macro turns an invalid lisp expression, (“hello, world” format t) into a valid form, (format t “hello, world”) which will be present in the run time version of the program:

(defmacro backwards (expr) (reverse expr))

(backwards (“hello world” format t))
;; at compile time this line is translated:
(reverse (“hello world” format t))
Then evaluated replacing the macro call line with this line in the runtime version of our script:
(format t “hello world”) 


Macros enable us to extend the lisp standard lib with custom syntax so long as we translate into valid s-expressions.

Macros enable us to abstract over common patterns of lisp code. For example, the `when` macro is a combination of an if and progn (which executes a sequence and returns the result of the final execution).
```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```

`unless` is the reverse of the when macro (executes a sequence only if the condition is not true).

cond is a macro for expressing multi-branch conditionals (if, else-if, else)

Then and and or macros implement logical conjunction and disjunction.

lisps looping constructs are also macros (do, dotimes, dolist)
Loop is another macro that provides a mini-language for expressing loops.

You can use return to break out of a dolist early:
```lisp
(dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
```

You can nest looping constructs.

do is the most general loop macro, and enables you to bind multiple variables:
```lisp 
  (do (variable-definition*)
      (end-test-form result-form*)
    statement*)
```

a variable definition is (name init-form step-form) (i 0 (+1 i))

when end-test-form evaluates to true, the result-form is evaluated then do ends.


