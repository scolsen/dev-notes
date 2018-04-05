(* Lists *)

(* As in Haskell, SML handles types using type and value constructors. *)
(* We can defined recursive and parameterized types, such as list. *)
(* A list can be of type nil (empty) or of type t :: t  (cons) *)

(* Sidenote: use the op operator to refer to a value constructor as a function
   This is useful for querying types. *)

(* :: is a right associative operation *)

(* We can also use list notation to define lists:
   [1, 2, 3, ..., n] *)

(* We can pattern match on lists. *)
(* The following length function computes the length of a list of any type t. *)

fun length nil = 0
  | length (_::t) = 1 + length t

(* Append two lists. This function is provided by SML *)
(* Has an infix form named @ *)
fun append (nil, l) = l
  | append (h::t, l) = h :: append (t, l)

(* One more example: reverse in O(n2) time. *)
fun rev nil = nil
  | rev h::t = rev t @ [h]

(* Better reverse using a helper function *)
local 
  fun helper (nil, a)  = a
    | helper (h::t, a) = helper (t, h::a)
in
  fun rev' l = helper (l, nil)
end

