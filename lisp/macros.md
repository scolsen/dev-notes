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

