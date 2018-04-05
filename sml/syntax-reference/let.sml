(* Let Syntax *)

(* Let blocks provide control over lexical binding in expressions. *)
(* Syntax: let <declaration> in <expression> end *)

let 
  val m = 3
in
  m * 2
end

let val m = 3 in m * 2 end

(* local syntax *)
(* Local blocks provide control over lexical binding in declarations *)
(* local declaration in declaration end *)

local 
  val r = 2
in 
  val k = r + 1
end
