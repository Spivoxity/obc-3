(*
 *  Oxford Oberon-2 compiler
 *  stack.ml
 *  Copyright (C) J. M. Spivey 1995, 1998
 *)

open Icode
open Symtab
open Print
open Dict
open Gcmap

(* We keep a stack of Booleans, each indicating whether an item on the
   evaluation stack at runtime is a pointer or not.  *)

let stk = ref []
let maxd = ref 0
let labstate = Hashtbl.create 100

let push_stack flag s = flag :: s

let rec pop_stack r s = Util.drop r s

let rec nth_stack s r = List.nth s r

let arity =
  let f = false and t = true in
  function
      ERROR (_, _) | STKMAP _ -> (0, [])
    | CONST _ | HEXCONST _ | TCONST (FloatT, _) -> (0, [f])
    | TCONST ((DoubleT|LongT), _) -> (0, [f; f])
    | (LOCAL _ | GLOBAL _) -> (0, [f])
    | LOAD (LongT|DoubleT) -> (1, [f; f]) | LOAD _ -> (1, [f])
    | CHECK (NullPtr, _) -> (1, [t])
    | ALIGN _ -> (1, [f])
    | BOUND _ | EASSERT _ -> (1, [])
    | POP n -> (n, [])
    | STORE (LongT|DoubleT) -> (3, []) | STORE _ -> (2, [])
    | FLEXCOPY -> (2, []) | FIXCOPY -> (3, [])
    | RETURN VoidT -> (0, [])
    | RETURN (DoubleT|LongT) -> (2, [])
    | RETURN _ -> (1, [])
    | TYPETEST _ -> (2, [f])
    | LINE _ -> (0, [])
    | CALL (n, VoidT) -> (n+1, []) 
    | CALL (n, (DoubleT|LongT)) -> (n+1, [f; f]) 
    | CALL (n, _) -> (n+1, [f]) 
    | (CHECK (GlobProc, _) | LINK) -> (1, [])
    | SAVELINK -> (0, [])

    | MONOP ((DoubleT|LongT), _) -> (2, [f; f])
    | CHECK (DivZero (DoubleT|LongT), _) -> (2, [f; f])
    | (MONOP (_, _) | CHECK (DivZero _, _)) -> (1, [f])

    | BINOP ((DoubleT|LongT), (Eq|Lt|Gt|Leq|Geq|Neq)) -> (4, [f])
    | BINOP ((DoubleT|LongT), _) -> (4, [f; f])
    | BINOP (PtrT, _) -> (2, [t])
    | BINOP (_, _) -> (2, [f])

    | CONV ((DoubleT|LongT), (DoubleT|LongT)) -> (2, [f; f])
    | CONV ((DoubleT|LongT), _) -> (2, [f])
    | CONV (_, (DoubleT|LongT)) -> (1, [f; f])
    | CONV (_, _) -> (1, [f])

    | JCASE _ -> (1, [])
    | JRANGE _ -> (3, [])

    | _ -> raise Not_found

let simulate i =
  begin match i with
      JUMP lab ->
	Hashtbl.add labstate lab !stk
    | TESTGE lab ->
	let s = pop_stack 1 !stk in
	Hashtbl.add labstate lab s; stk := s
    | DUP n ->
	stk := push_stack (nth_stack !stk n) !stk
    | JUMPC (_, _, lab) ->
	let s = pop_stack 2 !stk in
	Hashtbl.add labstate lab s; stk := s
    | SWAP ->
	let x = nth_stack !stk 0  and y = nth_stack !stk 1 in
	stk := push_stack y (push_stack x (pop_stack 2 !stk))
    | LABEL lab ->
	(* This assumes that a label immediately following an unconditional 
	   branch has an empty stack if it is not the target of some other 
	   forward branch. *)
	stk := (try Hashtbl.find labstate lab with Not_found -> [])

    | _ -> 
	(try
	  let (k, xs) = arity i in
	  stk := List.fold_right push_stack xs (pop_stack k !stk)
	with Not_found ->
	  failwith (sprintf "stack_sim $" [fInst i]))
  end;
  maxd := max !maxd (List.length !stk)

let reset () =
  Hashtbl.clear labstate; stk := []

let pop n = 
  stk := pop_stack n !stk

let mark () = 
  stk := push_stack true (pop_stack 1 !stk)

(* The stack map for a procedure call shows the pointer layout of the
eval stack of the calling procedure. It's based at (bp+HEAD+args), 
the address of the stack word just beyond the parameters of the
callee. Thus the stack map will be zero except in the rare case that
extra pointers stay on the stack of the caller throughout execution of
the callee.

When make_map is called, the n parameters (maybe including a static
link) and the code address of the callee are on the simulated stack;
so we drop n+1 stack items before computing the map. *)

let make_map n =
  let h p m = join (if p then ptr_map else null_map) (shift 4 m) in
  shift (4*n) (List.fold_right h (pop_stack (n+1) !stk) null_map)

let max_depth () = !maxd

let fStack = 
  let f prf = 
    let n = List.length !stk in
    for i = 0 to n-1 do 
      prf "$" [fChr (if nth_stack !stk i then 't' else 'f')]
    done in
  fExt f
