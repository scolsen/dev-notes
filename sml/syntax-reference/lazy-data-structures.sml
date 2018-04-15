(* Lazy Data Structures *)

(* By default, ML is an eager language, it uses a by-value binding approach. *)
(* All terms are evaluated before binding and function application *)

(* ML also provides lazy data types, to enable lazy, or by-need evaluation *)

(* Lazy eval is provided only by the SML/NJ compiler. *)
(* we must introduce the following top-level pragma to enable laziness *)

Compiler.Control.lazysml := true; 
open Lazy;

(* Use the lazy keyword to introduce a lazy datatype *)

datatype lazy 'a stream = Cons of 'a * 'a stream

(* we can construct values of lazy datatypes by using `val rec lazy` *)
val rec lazy ones = Cons (1, ones)

(* We can pattern match against lazy computations *)

val Cons (h, t) = ones

(* The prior pattern binds h to 1 and t to ones. *)
(* We can perform deeper matches if necessary. *)

val Cons (h, (Cons (h', t')) = ones 

(* Pattern matching forces evaluation of computations to the extent
   required by the pattern. *)

(* We can also declare lazy functions *)

fun lazy lstl (Cons (_, s)) = s

(* The above will defer performing pattern matching evaluation until required by another call. *)

(* Programming lazily *)
(* The following is a definition of a mapping function for a lazy stream data structure *)

fun smap f = 
  let 
    fun lazy loop (Cons (x, s)) = Cons (f x, loop s)
  in 
    loop
  end 

(* The prior function sets up a computation over a stream. *)

(* the following is an example usage of the smap function *)

val one_plus = smap (fn n => n + 1)
val rec lazy nats = Cons (0, one_plus nats)

(* The following function is a stream filter *)

fun sfilter pred = 
  let 
    fun lazy loop (Cons (x, s)) = 
      if pred x
      then Cons (x, loop s)
      else loop s
  in 
    loop
  end


