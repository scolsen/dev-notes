# Variables

There are two types of variables in lisp, lexical and dynamic.
Lisp is dynamically typed. There is no need to type variables.
Dynamic variables are also called special variables.
The let operator can be used to introduce variables:
```lisp
(let (variable-init-form*) body-form)
```
A variable init form is a list containing a variable name and an initial values, or just a variable name to set the value to nil:
```lisp
(let ((x 10) (y 20) z)...)
```
Variable defined in a let are only bound to their values within the let block, outside their bindings are set to whatever they may have been prior.

let and functions are considered binding-forms (variables are bound within these scopes)
Innermost binds in nested binding forms shadow any outer binds:
```lisp
(defun foo (x)
  (format t "~a~%" x) ; x is argument
  (let ((x 2))
    (format t "~a~%"))) ;x is 2
```
let\* is the same as let but enables you to refer to the bound variables outside of the let scope.

## Lexical Variables, Closures

Binding forms introduce lexically scoped variables by default.
Lexically bound variables support closures, i.e. a nested anonymous function can reference the local variable within the scope of its containing function. 
```lisp 
(let ((count 0)) #'(lambda () (setf count (1+ count))))
```
Closures capture bindings to variables, not just value assignment, and so reassignment of a var within a closure enables you to reassign the variable at each subsequent call of the closure:
```lisp
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (+1 count)))))
(funcall *fn*) -> 1
(funcall *fn*) -> 2
(funcall *fn*) -> 3 ...
```
In other words, the value of a closed var persists between calls.
We can also return a closures:
```lisp 
(let ((count 0))
  (list 
    #'(lambda () (incf count))
    #'(lambda () (decf count))
    #'(lambda () count)))
```

## Dynamic or Special Variables

Lisp provides two ways to create global vars: defparameter and defvar both take a name, an initial value, and a documentation string

by convention, global variables begins with *
```lisp
(defvar count 0 
  "A count of something")
```
defparameter always assigns the initial value to the name, defvar will only assign the initial value if the name is undefined (safe)
You can also use defvar to set a global with no initial value, this is called an *unbound* variable.
you can refer to gloabls defined with either of the prior functions from anywhere in lisp code.
to reset a defvar variable use setf or makunbound to make it unbound.

Dynamic variables enable you to redefine an existing variable within a limited scope, for temporary use. After the scope is exited, the dynamic var returns to its original value. All global variables are dynamic variables, and can be modified for the duration of the scope of a binding form using binders such as let. To temporarily redefine \*standard-output\*:
```lisp
(let ((*standard-output* *some-other-stream*))
  (doStuff))
```
Any code reference by doStuff that uses \*standard-output\* will instead use \*some-other-stream\* for the duration of that call.

At any time, the most recent binding shadows all other bindings.
binding forms use a stack and pop off as resolved.
The interval during which references can be made to a particular bind is called its *extent*.
Multi threading messes up this dynamism, as if multiple threads try to rebind the variable the program becomes confused. Some lisp implementations resolve this create dynamic bindings per thread.

only special vars can be dynamic, and must be declared using defvar and defparameter. The \*naming\* convention is intended to illustrate the variable is dynamic. lets will always bind special variables dynamically. 

It is also possible to declare locally special vars, but this is rare.

## Constants

Define constants with `defconstant`. All constants are global.
`(defconstant name init-value "doc string")`
It is conventional to begin constant names with a +

## Assignment

Use `setf` to assign values to binds (vars):
`(setf name)`
To get the value of the variable, simply reference the name.
setf only rebinds within its scope.
You can assign multipl values:
`(setf x 1 y 2)`
Or nest setf to assign one value to multiple names:
`(setf x (setf y 10))`
Setf can assign values to multiple structures, not just variable names.


