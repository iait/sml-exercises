(* Straight Line Program Interpreter *)

val _ = load "Int";

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

(*** a := 5 + 3; b := (print(a, a - 1), 10 * a); print(b) ***)
val prog =  
  CompoundStm(
    AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(
      AssignStm("b", EseqExp(
        PrintStm[IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
        OpExp(NumExp 10, Times, IdExp "a"))),
      PrintStm[IdExp "b"]))

(*** maxargs : stm -> int ***)
local
  fun maxArgsExp (IdExp _) = 0
    | maxArgsExp (NumExp _) = 0
    | maxArgsExp (OpExp (exp1, _, exp2)) = Int.max (maxArgsExp exp1, maxArgsExp exp2)
    | maxArgsExp (EseqExp (stm, exp)) = Int.max (maxArgsStm stm, maxArgsExp exp)
  and maxArgsStm (CompoundStm (stm1, stm2)) = Int.max (maxArgsStm stm1, maxArgsStm stm2)
    | maxArgsStm (AssignStm (_, exp)) = maxArgsExp exp
    | maxArgsStm (PrintStm xs) = 
        let val listMax = foldl Int.max 0
        in listMax (length xs :: map maxArgsExp xs) end 
in
  val maxargs = maxArgsStm
end

(*** interp : stm -> unit ***)
local
  type table = (id * int) list
  
  (*** update : table * id * int -> table ***)
  fun update (t : table, id, value) = (id, value)::t

  (*** lookup : table * id -> int ***)
  fun lookup ((id1, value)::ts : table, id2) = 
    if id1 = id2 then value 
                 else lookup (ts, id2)

  (*** interpStm : stm * table -> table ***)
  fun interpStm (CompoundStm (stm1, stm2), t) = interpStm (stm2, interpStm (stm1, t))
    | interpStm (AssignStm (id, exp), t1) = 
        let val (value, t2) = interpExp (exp, t1)
        in update (t2, id, value) end
    | interpStm (PrintStm l, t1) = 
        let fun aux (exp, t2) =
                  let val (value, t3) = interpExp (exp, t1)
                  in t3 before print ((Int.toString value) ^ " ") end
        in foldl aux t1 l before print "\n" end

  (*** interpExp : exp * table -> int * table ***)
  and interpExp (IdExp id, t) = (lookup (t, id), t)
    | interpExp (NumExp value, t) = (value, t)
    | interpExp (OpExp (exp1, binop, exp2), t1) = 
        let
          val (value1, t2) = interpExp (exp1, t1)
          val (value2, t3) = interpExp (exp2, t2)
        in
          case binop of Plus => (value1 + value2, t3)
                      | Minus => (value1 - value2, t3)
                      | Times => (value1 * value2, t3)
                      | Div => (value1 div value2, t3)
        end
    | interpExp (EseqExp (stm, exp), t) = interpExp (exp, interpStm (stm, t))
in
  fun interp stm = () before interpStm (stm, [])
end
