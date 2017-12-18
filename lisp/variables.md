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

