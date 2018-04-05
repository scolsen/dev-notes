(* Polymorphic Types *)
(* For example, to define identity, which is defined for infinitely many types: *)
val I : 'a -> 'a = fn x => x

(*Named version *)
fun I (x:'a):'a = x

(* The right hand side of a polymorphic type must always be a value *)
