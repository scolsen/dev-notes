(* Standard ML Function Syntax *)

(* Anonymous Functions *)

fn x : int => 3 + x

(* Named Functions *)

fun concat (s:string):string = s ^ " world"

(* Parameter Shadowing *)
(* Bindings in sml are lexical, therefore, smaller scopes within function bodies may shadow function parameters *)
(* In  the following example, x is set to two for the scope of the let block, then restored to the argument value after the let concludes *)
fun h(x:int):int =
  let val x:int = 2 in x + x end * x

(* Functions may refer to bindings in prior/outer/higher scopes *)
val x:int = 3
fun g(y:int):int = x + y (* x refers to the top level x in this function, and resolves to 3*)

(* functions can make use of pattern matching over heterogenous types (concretes) in order to provide clauses or gaurds *)
(* clauses are evaluated top down. *)
fn 0 => 0 | n:int => 1 div n

fn 0 => 0
  | n:int => 1 div n

(* Named function with gaurds *)
fun recip 0 = 0
  | recip (n: int) = 1 div n

(* Constructs like case and if are secretly functions with gaurds, here is if expressed as a function *)
(fn true => true | false => false)

(* Gaurds are checked for redundancy and exhuastiveness *)
(* As with haskell, _ is used to attain complete match coverage *)
fun numerals #"1" = true
  | numerals #"2" = true
  | numerals #"3" = true
  | numerals _ = false

(* Recursive value bindings are qualified by the keyword rec *)
val rec factorial : int -> int =
  fn 0 => 1 | n:int => n * factorial (n - 1)

(* rec is implicit in named function deifnitions *)
fun factorial 0 = 0
  | factorial (n:int) = n * factorial (n - 1)

(* Accumulator/iterative/tail recursion. It is good style to define helper functions in local blocks *)
local 
  fun helper (0, r:int) = r
    | helper (n:int, r:int) = helper (n - 1, n * r)
in
  fun factorial (n:int) = helper (n, 1)
end

(* Mututal recursion using the and keyword*)
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)


