(* Mutable Storage *)

(* To enable the storage and retrival of values, SML has mutable cells. 
   Mutable cells are fixed in number, and enable imperative functionality. 
   SML's references cells are mutable. They have type typ ref.
   Reference cells are allocated using the ref function.
   We retrieve the contents of a ref using the ! function.
   We change the contents of a ref using := (assignment)*)

val r = ref 0
val s = ref 0
val _ = r := 3
val = !s + !r

(* We use val _ when an expression is evaluated purely for its effects *)

(* Functions of the type typ -> () are called proceedures.
   Proceedures are evaluated only for their effects. 
   Val _ sequences effectful computations. *)

(* we can also pattern match refs to get their values *)

fun me (ref a) = a

(* ; also indicates sequential computation without the val _ pattern *)

val r = ref 0
val s = ref 0
(s := 5; !r)

(* Equality for the reference type is defined as reference equality
   For two terms that refer to a ref type to be equal, 
   they must refer to the same ref cell *)


