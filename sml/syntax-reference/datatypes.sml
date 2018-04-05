(* Datatype Declarations *)
(* As in Haskell, SML provides datatype declarations. 
   Unlike type synonyms, datatypes are semantically distinct.
   We can create our own type constructors (recursive included)
   using datatype declarations. *)
(* Type constructors can be mutually recursive.
   Each type constructor must have a value constructor.
   Type constructors can take arguments. 
   Value constructors can also take arguments. *)

(* A simple non-recursive sum datatype: *)
(* It is conventional to capitalize values constructors
   but it is not required. *)

datatype suit = Hearts | Diamonds | Spades | Clubs

(* We can pattern match on our type's values. *)

fun outranks (Spades, Spades) = false
  | outranks (Spades, _) = true
  | outranks (Hearts, Spades) = false
  | outranks (Hearts, Hearts) = false
  | outranks (Hearts, _) = true
  | outranks (Diamonds, Clubs) = true
  | outranks (Diamonds, _) = false
  | outranks (Clubs, _) = false

(* We can also create parameterized types. *)
(* Note the use of 'of' to apply the type argument. *)

datatype 'a option = None | Some of 'a

(* e.g. the type 'string option' could be None or Some "abc" *)
(* The option type is defined in SML. 
   It can be used to handle optional arguments. *)

fun expt (NONE, n) = expt (SOME 2, n)
  | expt (SOME b, 0) = 1
  | expt (SOME b, n) = 
      if n mod 2 = 0
      then expt (SOME (b*b), n div 2)
      else b * expt (SOME b, n-1)

(* Returning an option *)

fun reciprocal 0 = NONE
  | reciprocal n = SOME (1 div n)

(* We can also define recursive types, such as a binary tree. *)

datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* We can process the tree recursively. *)

fun height Empty = 0
  | (Node (l, _, r)) = 
      1 + max (height l, height r)

(* It is wise to capitalize value constructors
   Lowercase value constructors can cause ambiguity
   For example, empty in the following case refers to a variable not a value 
   And will erroneously match trees that have nodes at the empty clause 
   yielding a constant result of 0 *)

fun size empty = 0
  | size (Node (l, _, r) =
      1 + size l + size r

(* We could also define a variadic tree utilizing lists or mutual recursion *)

datatype 'a vtree = Empty
                  | Node of 'a * 'a vtree list

datatype 'a vtree' = Empty 
                   | Node of 'a * 'a forest
and 'a forest = None
              | Tree of 'a vtree' * 'a forest

(* We must then define mutually recursive functions
   to process mutually recursive types. *)
fun size_tree Empty = 0
  | size_tree (Node (_, f)) = 1 + size_forest f
and size_forest None = 0
  | size_forest (Tree (t, f')) = size_tree t + size_forest f'

(* We can also define heterogenous types. *)

datatype int_or_string = Int of int
                       | String of string

(* we can then concretize a form of tree with this data type *)

type int_or_string_tree = int_or_string tree

(* Pattern matches over types must be exhaustive *)

