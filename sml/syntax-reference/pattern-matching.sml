(* Sml supports pattern matching to match the values of higher order structures *)

(* Pattern matching Tuples *)
(* Tuple patterns enable us to bind values using tuples *)
(* Underscores may be used to indicate holes, values which will not be bound *)
(* Pattern binding must match the form val pattern = expression *)
val (_, _, r:int) = (0, 0, 3) (* binds r to value 3)
val (t: int * int, _) = ((4, 5), "help")
val ((i:int, s:string), r:real, m:int) = ((5, "hello"), 3.0, 4) (* binds each variable to the tuple values *)

(* We can also pattern match on records *)

type address = { number: int, street: string, state: string}
val home: address = {number=345, street="Maple", state="PA"}

val {number=n, street=s, state=st} = home (* binds n, s, and st, to the respective parts of hom *)

(* ellipses patterns can be used to ignore a swath of structure contents in a record with many fields *)
val {number=mynum, ...} = home
(* The elpsis binds all other record fields to _ *)

(* If you intend to name the variables using the same name as the labels of the record, you can use a shortend pattern binding syntax *)
val {number, street, state} = home (* binds name number to number record value, street to street record value, state to state record value. *)

(* Functions can be defined using valid patterns *)
(* fn pattern => expression *)
(* For example, we declare higher arity functions using a tuple pattern *)
fun f (x:real, y:real):real = (x * x + y * y)
(* we then call against an argument tuple *)
f (2.3, 5.6)

(* we can use record patterns to provide keyword arguments *)
fun g {x=x:real, y=y:real} = (x * x + y * y)
g {x=2.0, y=56.4}

(* Functions may return tuples to return multiple results *)
fun h (x:real, y:real) = (x*x, y*y)

(* as in elixir, we can also match against concretes *)
val 0 = 1-1 (* results in true *)


