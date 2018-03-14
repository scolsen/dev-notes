(* Let Syntax *)

(* let dec in exp end *)

let 
  val m = 3
in
  m * 2
end

let val m = 3 in m * 2 end

(* local syntax *)

(* local dec in dec end *)

local 
  val r = 2
in 
  val k = r + 1
end

local val r = 2 in val k = r + 1 end
