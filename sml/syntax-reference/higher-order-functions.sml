(* Higher Order Functions *)

(* Functions use the closest lexical binding, even if the value is redeclared *)

val x = 2
fun f y = x + y
val x = 3
val z = f 4 (* Binds to 6, not 7, since 2 is a closer lexical closure to f *)

(* Map, of course, is a go-to example of a higher-order function. *)

fun map' (f, nil) = nil
  | map' (f, h::t) = (f h) :: map' (f, t)

(* The constantly function yields constant functions *)

val constantly = fn k => (fn a => k)

(* As in haskell, function application is implict, 
   so we can express this tersely *)

fun constantly  k a = k

(* Here is a better definition of map, utilizing currying 
   This is better form than using tuples. *)

fun map f nil = nil
  | map f (h::t) = (f h) :: (map f t)

(* And here's map using the curry function explicitly with tuple-arg map
   Curry has type: (a*b->c)->(a->(b->c)) *)

fun map'' f l = curry map' f l

(* Function application associates left. *)

(* We can often return functions to improve function performance.
   Two definitions of reduce follow. One constructs tuples each iteration 
   The other uses a helper function to maniuplate only the variable elements. *)

fun reduce (operation, unit, nil) = unit
  | reduce (operation, unit, (h::t)) = operation h (reduce (operation, unit, t))

fun reduce' (oper, unit, l) = (* This is more performant. *)
  let
    fun red nil = unit
      | red (h::t) = oper h (red t)
  in
    red l
  end

(* We can improve the function even further
   Observing that unit and operation remain the same for many lists
   It is better to create a reduce that simply returns a function 
   for computing a reduction using a specified unit and operation
   that may be called later on n lists. *)

fun stage_reduce (unit, oper) = 
  let 
    fun red nil = unit
      | red (h::t) = oper h (red t)
  in
    red
  end

