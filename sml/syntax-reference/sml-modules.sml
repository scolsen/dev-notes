(* ML provides a module system for compositional programming. *)
(* Modules are defined in terms of signatures, which are implemented by structures *)

(* A sig may describe many structures, and structures may implement multiple sigs *)
(* A signature specifies constraints on a structure *)

(* A signature consists of the following form:
   sig <specs>
   where specs may be:
   a type specification
   a datatype specification
   an exception specification
   a value specification 
   We name signature using a signature binding of the form:
   signature <sigid> = <sigexpression> *)

(* The following is a sample signature definition *)

signature QUEUE =
  sig 
    type 'a queue
    exception Empty 
    val empty : 'a queue
    val insert : 'a * 'a queue -> 'a queue 
    val remove : 'a queue -> a' * a' queue 
  end

(* we can conceive of signatures as less elegant type classes. *)

(* Structures implement signatures. The types of structures are signatures. *)
(* Structures follow the syntax struct <declarations> end. *)
(* Struct declarations can be type declarations, datatype declarations,
   exception declarations, or a value declaration. *)

structure Queue = 
  struct 
    type 'a queue = 'a list * 'a list
    exception Empty
    val empty = (nil, nil)
    fun insert (x, (b,f)) = (x :: b, f)
    fun remove (nil, nil) = raise Empty
      | remove (bs, nil) = remove (nil, rev bs)
      | remove (bs, f::fs) = (f, (bs, fs))
  end

(* funs are technically val rec bindings *)

(* structure components are accessed using identifiers. *)

Queue.empty
val q = Queue.insert (1, ([1,2,3], [1,2,3]))

(* We can abbreviate strucutre definitions by binding them to other structures *)

structure Q = Queue

(* We can also open structures to load their bindings directly into the environment. *)

open Queue 

(* we can then simply call empty, insert, etc. *)
(* open can take multiple structures, if there are overlapping definitions *)
(* the last loaded structure's definitions will be used in the case of a conflict. *)
(* Open will also shadow names in the environment. *)
(* use open very sparingly. *)


