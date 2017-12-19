# Macros

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

## Defining Macros

Macro expansion happens during *macro expansion time*, a compiler step which preceeds runtime.

Macro expansion time thus has no access to data the only exists at runtime.

*It is important to keep the macro expand time/runtime in mind*

Macros work on lists that represent some bit of lisp source code.

For example, here is a use of the when macro:
```lisp
;;; when is defined as (defmacro when (condition &rest body)
;;;                      `(if ,condition (progn ,@body)))

(when ((> x 10) (print 'big)))
;; when evaluated transforms to:
(if (> x 10) (progn (print 'big)))
```

If a lisp is interpreted instead of compiled, macro expand time and runtime are temporarily intertwined.

use `defmacro` to define a macro. Defmarco is defined as:
(defmacro name (parameters)
  "Optional doc string."
  body-forms)

It's helpful to design macros from the bottom-up--find a repetitive pattern, then abstract it out into a macro. We need to know how it's going to be called, what it's producing, and why. 

We need an *example call* and a *desired expansion*.

It is possible for macros to 'leak'. We must avoid macro leaks. 

Macros parameter lists are *destructuring* parameter lists. Destructuring params can be replaced with a nested parameter list, and each element of the list will map to an element of the passed structure. 

For example, to capture three elements of a list you can just set (x y z) as the parameter value to assign each name to the elements of the list within the function.

&body can also be used as a synonym for &rest within macro parameter lists.

Destructured params will also throw errors if given an arg of invalid length.

destructured params can contain nested destructured params, as well as &optional, &key, and &rest.

the `@` operator splices an unquoted name in an expression into a list, i.e. ,@body will evaluate body and translate the contents into a list.

The backtick effectively works as a list generator operator.
\`(,a b) translates to (list a 'b)

use macroexpand-1 to have lisp return the expansion of a macro form. The macro form must be quoted with the quote operator.

don't evaluate forms passed to a macro more times than necessary.
(trust)


You must also avoid evaluating name sout of order in a macro (based on when they are called).

Macros must also use unique names to avoid conflicts with passed body-forms. Use `gensym`, which generates a sunique symbol on each call, for this purpose.

a gensym symbol is prefixed with #: which means "uninterned" symbol:
\#:g2141

## Rules for avoiding leaks

* Include subforms in the expansion in the same position as the subforms are given in the macro call.
* Make sure subforms are evaluated only once. Create a var to hold the value of the evaluated form and use that value where needed.
* Use gensym to create unique variables within the expansion.

### Example Macro doprimes

We can write an example looping construct over primes to demonstrate a macro.
```lisp
(defun is-prime (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (is-prime n) (return n)))

(defmacro doprimes ((var start end) &body body)
  `do ((,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ,end))
    ,@body)
```

## Macro Generating Macros

You can also write macros that generate macros. For example, with-gensym:
```lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))
```


