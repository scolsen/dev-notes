(* Exceptions *)

(* Exceptions are raised on dynamic violations or errors. *)

(* We define new exceptions using the exception declaration.
   We throw exceptions by using the raise expression. *)

exception Factorial

local
  fun fact 0 = 1
    | fact n = n * fact (n - 1)
in
  fun checked_fact n =
    if n >= 0 
    then fact n
    else raise Factorial

(* We can catch exceptions with exception handlers. *)
(* We use the handle expression to handle exceptions. *)
(* Handlers are of the form: expr `handle` match *)

fun factorial_driver () =
  let 
    val input = read_integer ()
    val result = toString (checked_fact input)
  in
    print result
  end (* end of expr *)
  handle Factorial => print "Out of range." (* end of match *)

(* Computation is terminated on uncaught exceptions *)

(* We can also handle multiple exceptions with a single handler by matching *)

(1 + 1) handle Factorial => print "Factorial."
             | Otherexception => print "Something else."

(* We can also attach values to exceptions to provide further context *)

exception SyntaxError of String

raise SyntaxError "Illegal character."

(* All exceptions have the type exn, the exception packet type. *)


