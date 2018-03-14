(* SML supports Product Types *)

(* Tuples are collections of N values of the any type *)
(* Tupes have the type signature type * type * type ... *)
val m: int * int = (1, 2)
val s: int * int * string = (1, 2, "pair")

(* You can also nest tuples, and their type declarations *)
val t: (int * int) * string = ((1, 2), "pair")

(* There is an empty or null tuple, which contains no values *)
(* The empty tuple has type unit *)
val u: unit = ()

(* One value tuples do not exist *)

(* Tuples may also contain expressions, which are evaluated from left to right *)
val e: int * int = (3 + 2, 2 + 3)

(* Getting Tuple values. *)
(* The convienence function #i can access a tuple element directly *)
(* E.g. in a function that takes a tuple of args #1 will reference the first element of the tuple *)
(* This syntax is discouraged *)
fun d (p:real*real):real = ((#1 p)+(#2 p), (#1 p)*(#2 p))


