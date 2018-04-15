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

