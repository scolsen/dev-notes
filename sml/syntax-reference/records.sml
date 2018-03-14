(* SML Record Syntax *)

(* record types provide labels for thier components *)
(* Binding particular record compositions to a type is a frequent pattern *)

type address = { number: int, street: string, state: string}
val home: address = {number=345, street="Maple", state="PA"}

(* As with tuples, SML provides a shortcut for accessing record values by label *)
(* The ellipses are required to make the record type unabmiguous *)
fun #prod {prod=x, ...} = x

