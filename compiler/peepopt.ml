(*
 * peepopt.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Symtab
open Icode
open Print
open Error
open Eval

let rkind = ref VoidT

type labdata = LabDef of labrec | Equiv of codelab
and labrec = { y_id : codelab; y_refct : int ref; mutable y_return: bool }

let label_tab = Hashtbl.create 257

let get_label x =
  try !(Hashtbl.find label_tab x) with
    Not_found -> 
      let y = LabDef { y_id = x; y_refct = ref 0; y_return = false } in
      Hashtbl.add label_tab x (ref y); y

let rec find_label x =
  match get_label x with
      LabDef y -> y
    | Equiv x' -> find_label x'

let rename x = let y = find_label x in y.y_id

let ref_count x = let y = find_label x in y.y_refct

let same_lab x1 x2 =
  let y1 = find_label x1 and y2 = find_label x2 in
  y1.y_id = y2.y_id

let equate x1 x2 =
  if !Config.debug > 1 then
    printf "! equate $ = $\n" [fLab x1; fLab x2];
  let y1 = find_label x1 and y2 = find_label x2 in
  if y1.y_id = y2.y_id then failwith "equate";
  y2.y_refct := !(y1.y_refct) + !(y2.y_refct);
  y2.y_return <- y1.y_return || y2.y_return;
  Hashtbl.find label_tab y1.y_id := Equiv y2.y_id  

let is_return x =
  let y = find_label x in y.y_return

let set_return x =
  let y = find_label x in y.y_return <- true

let do_refs f =
  function
      JUMP x -> f (ref_count x)
    | JUMPB (b, x) -> f (ref_count x)
    | JUMPC (t, w, x) -> f (ref_count x)
    | JUMPCZ (w, x) -> f (ref_count x)
    | TESTGEQ x -> f (ref_count x)
    | JCASE labs ->
	List.iter (function x -> f (ref_count x)) labs
    | JRANGE x -> f (ref_count x)
    | _ -> ()

let rename_labs =
  function
      LABEL x -> LABEL (rename x)
    | JUMP x -> JUMP (rename x)
    | JUMPB (b, x) -> JUMPB (b, rename x)
    | JUMPC (t, w, x) -> JUMPC (t, w, rename x)
    | JUMPCZ (w, x) -> JUMPCZ (w, rename x)
    | TESTGEQ x -> TESTGEQ (rename x)
    | JCASE labs -> JCASE (List.map rename labs)
    | JRANGE x -> JRANGE (rename x)
    | i -> i

let is_label = function LABEL _ -> true | _ -> false

let ruleset1 replace =
  function
    (* Temporarily expand abbreviations *)
      JUMPC (k, w, lab) :: _ ->
	replace 1 [BINOP (k, w); JUMPB (true, lab)]
    | MONOP (IntT, Inc) :: _ ->
	replace 1 [CONST (integer 1); BINOP (IntT, Plus)]
    | MONOP (IntT, Dec) :: _ ->
	replace 1 [CONST (integer 1); BINOP (IntT, Minus)]
    | BINOP (IntT, BitSub) :: _ ->
	replace 1 [MONOP (IntT, BitNot); BINOP (IntT, BitAnd)]

    (* Static bounds checks *)
    | CONST n1 :: CONST n2 :: BOUND _ :: _ 
	  when n1 >= integer 0 && n1 < n2 -> 
	replace 3 [CONST n1]
    | CONST n :: CHECK (DivZero IntT, _) :: _ when n <> integer 0 ->
	replace 2 [CONST n]
    | TCONST (LongT, n) :: CHECK (DivZero LongT, _) :: _ 
	when int_value n <> integer 0 -> replace 2 [TCONST (LongT, n)]
    | TCONST (FloatT, x) :: CHECK (DivZero FloatT, _) :: _
	when flo_value x <> 0.0 -> replace 2 [TCONST (FloatT, x)]
    | TCONST (DoubleT, x) :: CHECK (DivZero DoubleT, _) :: _
	when flo_value x <> 0.0 -> replace 2 [TCONST (DoubleT, x)]
    | CONST n :: CHECK (GlobProc, _) :: _ ->
	if n = integer 0 then replace 2 []
    | GLOBAL x :: CHECK (NullPtr, _) :: _ ->
	replace 2 [GLOBAL x]

    (* A little constant folding *)
    | CONST a :: CONST b :: BINOP (IntT, w) :: _ ->
	replace 3 [CONST (int_binop w a b)]
    | LOCAL o :: CONST n :: BINOP (PtrT, PlusA) :: _ ->
	replace 3 [LOCAL (o + int_of_integer n)]
    | CONST a :: MONOP (IntT, w) :: _ ->
	replace 2 [CONST (int_monop w a)]
    | CONST a :: JUMPB (true, lab) :: _ ->
	replace 2 (if a <> integer 0 then [JUMP lab] else [])
    | CONST a :: JUMPB (false, lab) :: _ ->
	replace 2 (if a = integer 0 then [JUMP lab] else [])

    | CONST a :: BINOP (IntT, Minus) :: _ ->
	replace 2 [CONST (integer_neg a); BINOP (IntT, Plus)]
    | CONST a :: BINOP (IntT, Plus) :: CONST b :: BINOP (k, Plus) :: _ ->
	replace 4 [CONST (integer_add a b); BINOP (IntT, Plus)]
    | CONST a :: BINOP (PtrT, PlusA) :: CONST b :: BINOP (PtrT, PlusA) :: _ ->
	replace 4 [CONST (integer_add a b); BINOP (PtrT, PlusA)]
    | CONST n :: BINOP (IntT, Plus) :: _ when n = integer 0 ->
	replace 2 []
    | CONST n :: BINOP (PtrT, PlusA) :: _ when n = integer 0 ->
	replace 2 []
    | CONST n :: BINOP (IntT, BitAnd) :: _ when n = integer (-1) ->
	replace 2 []

    | CONST a :: BINOP (IntT, Times) :: CONST b :: BINOP (IntT, Times) :: _ ->
	replace 4 [CONST (integer_mul a b); BINOP (IntT, Times)]
    | CONST n :: BINOP (IntT, Times) :: _ when n = integer 1 ->
	replace 2 []

    | MONOP (BoolT, Not) :: JUMPB (b, lab) :: _ ->
	replace 2 [JUMPB (not b, lab)]
    | BINOP (k, w) :: MONOP (BoolT, Not) :: _ ->
	begin
	  try let w' = opposite w in replace 2 [BINOP (k, w')]
	  with Not_found -> ()
	end

    (* Convert int to char or short, then store char *)
    | CONV (IntT, (CharT | ShortT)) 
	  :: (LOCAL _ | GLOBAL _) :: STORE CharT :: _ -> 
	replace 1 []
    | CONV (IntT, ShortT) :: (LOCAL _ | GLOBAL _) :: STORE ShortT :: _ -> 
	replace 1 []

    (* Specially for INC(local) and INC(global) *)
    | LOCAL a :: DUP 0 :: LOAD s :: CONST n :: BINOP (IntT, Plus)
	  :: SWAP :: STORE s1 :: _  when s = s1 ->
	replace 7 [LOCAL a; LOAD s; CONST n; 
	  		BINOP (IntT, Plus); LOCAL a; STORE s]
    | GLOBAL x :: DUP 0 :: LOAD s :: CONST n :: BINOP (IntT, Plus)
	  :: SWAP :: STORE s1 :: _  when s = s1 ->
	replace 7 [GLOBAL x; LOAD s; CONST n; 
	      		BINOP (IntT, Plus); GLOBAL x; STORE s]

    (* For simultaneous assignment *)
    | (LOCAL _ | GLOBAL _ as i1) :: CONST b :: 
	  (LOCAL _ | GLOBAL _ as i2) :: STORE s :: _ ->
	replace 4 [CONST b; i2; STORE s; i1]
    | (LOCAL _ | GLOBAL _ as i1) :: (LOCAL _ | GLOBAL _ as i2) :: 
	  LOAD s1:: (LOCAL _ | GLOBAL _ as i3) :: STORE s2 :: _ ->
	replace 5 [i2; LOAD s1; i3; STORE s2; i1]

    (* Boolean equality comparisons *)
    | CONST n :: BINOP (BoolT, (Eq | Neq as w)) :: _ ->
	let w' = if n = integer 0 then w else opposite w in
	replace 2 (if w' = Neq then [] else [MONOP (BoolT, Not)])

    (* Void returns *)
    | CALL (n, k) :: POP s :: _ when k <> VoidT ->
	replace 2 [CALL (n, VoidT)]
    | CONST n :: POP 1 :: _ ->
	replace 2 []

    (* Unneeded static links *)
    | CONST n :: LINK :: _ when n = integer 0 ->
	replace 2 []

    (* Tidy up jumps and labels: *)
    | JUMP lab :: LABEL lab1 :: _ when same_lab lab lab1 ->
	replace 1 []
    | JUMP lab :: i :: _ when not (is_label i) -> 
	replace 2 [JUMP lab]
    | RETURN s :: i :: _ when not (is_label i) -> 
	replace 2 [RETURN s]
    | JUMPB (b, lab1) :: JUMP lab2 :: LABEL lab1a :: _
	  when same_lab lab1 lab1a ->
	replace 2 [JUMPB (not b, lab2)]
    | LABEL lab1 :: JUMP lab2 :: _ when not (same_lab lab1 lab2) ->
	replace 1 []; equate lab1 lab2
    | LINE n :: JUMP lab2 :: _ -> 
	replace 1 []
    | LABEL lab1 :: LABEL lab2 :: _ ->
	replace 2 [LABEL lab1]; equate lab2 lab1
    | LABEL lab :: _ when !(ref_count lab) = 0 ->
	replace 1 []
    | LINE _ :: LINE _ :: _ -> 
	replace 1 []
    | LINE n :: LABEL lab :: _ ->
	replace 2 [LABEL lab; LINE n]
    | LABEL lab :: RETURN s :: _ when not (is_return lab) ->
        rkind := s; set_return lab; replace 2 [LABEL lab; RETURN s]
    | JUMP lab :: _ when is_return lab ->
        replace 1 [RETURN !rkind]

    | _ -> ()

let simple =
  function
    CONST _ | LOCAL _ | GLOBAL _ -> true
  | LDL ((CharT|ShortT|IntT|FloatT), x) -> true
  | LDG ((CharT|ShortT|IntT|FloatT), x) -> true
  | _ -> false

let width =
  function
       CharT | BoolT | ByteT -> 1 
     | ShortT -> 2
     | IntT | FloatT | PtrT -> 4
     | LongT | DoubleT -> 8
     | _ -> failwith "width"

let divisible n s =
  integer_mod n (integer (width s)) = integer 0

let divide n s =
  integer_div n (integer (width s))

let ruleset2 replace =
  function
      (* Introduce specialized instructions *)
      CONST n :: BINOP (IntT, Plus) :: _ when n = integer 1 ->
	replace 2 [MONOP (IntT, Inc)]
    | CONST n :: BINOP (IntT, Plus) :: _ when n = integer (-1) ->
	replace 2 [MONOP (IntT, Dec)]
    | CONST n :: BINOP (IntT, Plus) :: _ when n < integer 0 ->
	replace 2 [CONST (integer_neg n); BINOP (IntT, Minus)]
    | BINOP (k, (Eq|Neq|Gt|Lt|Geq|Leq as w)) :: JUMPB (b, lab) :: _ ->
	replace 2 [JUMPC (k, (if b then w else opposite w), lab)]

    | LOCAL n :: LOAD s :: _ -> replace 2 [LDL (s, n)]
    | LOCAL n :: STORE s :: _ -> replace 2 [STL (s, n)]

    | LDL (IntT, n) :: MONOP (IntT, Inc) :: STL (IntT, n1) :: _ when n = n1 ->
	replace 3 [INCL n]
    | LDL (IntT, n) :: MONOP (IntT, Dec) :: STL (IntT, n1) :: _ when n = n1 ->
	replace 3 [DECL n]

    | CONST n :: JUMPC (IntT, w, lab) :: _ when n = integer 0 ->
	replace 2 [JUMPCZ (w, lab)]
    | DUP 0 :: CONST n :: JUMPC (IntT, Geq, lab) :: _ ->
	replace 3 [CONST n; TESTGEQ lab]

    | CONST s :: BINOP (IntT, Times) :: BINOP (PtrT, PlusA) :: _
	  when s = integer 2 || s = integer 4 || s = integer 8 ->
	replace 3 [INDEX (int_of_integer s)]

    | CONST n :: BINOP (PtrT, PlusA) :: LOAD s :: _
          when divisible n s ->
        replace 3 [CONST (divide n s); LDI s]
    | CONST n :: BINOP (PtrT, PlusA) :: STORE s :: _
          when divisible n s ->
	replace 3 [CONST (divide n s); STI s] 

    | CONST n :: BINOP (IntT, Times) :: BINOP (PtrT, PlusA) :: LOAD s :: _
          when divisible n s ->
	replace 4 [CONST (divide n s); BINOP (IntT, Times); LDI s]
    | CONST n :: BINOP (IntT, Times) :: BINOP (PtrT, PlusA) :: STORE s :: _
          when divisible n s ->
	replace 4 [CONST (divide n s); BINOP (IntT, Times); STI s]

    | BINOP (PtrT, PlusA) :: LOAD CharT :: _ ->
	replace 2 [LDI CharT]
    | BINOP (PtrT, PlusA) :: STORE CharT :: _ ->
	replace 2 [STI CharT]
    | INDEX n :: LOAD s :: _ when n = width s ->
	replace 2 [LDI s]
    | INDEX n :: STORE s :: _ when n = width s ->
	replace 2 [STI s]

    | CONST n :: LDI IntT :: _ -> 
	replace 2 [LDNW (4 * int_of_integer n)]
    | CONST n :: STI IntT :: _ -> 
	replace 2 [STNW (4 * int_of_integer n)]

    | LDL (IntT, -4) :: LDNW n :: _ ->
	replace 2 [LDEW n]
    | LDL (IntT, -4) :: STNW n :: _ ->
	replace 2 [STEW n]

    (* Eliminate simple swaps *)
    | i1 :: i2 :: SWAP :: _  when simple i1 && simple i2 ->
	replace 3 [i2; i1]

    | _ -> ()

let fits n x = 
  let max = 1 lsl (n-1) in -max <= x && x < max

let fits64 n x = 
  let max = integer_lsl (integer 1) (n-1) in 
  integer_neg max <= x && x < max

let ruleset3 replace = 
  function
      (* Avoid out-of-line constants *)
      TCONST (LongT, x) :: _ when fits64 16 (int_value x) ->
	replace 1 [CONST (int_value x); CONV (IntT, LongT)]

      (* Introduce LDG/STG *)
    | GLOBAL x :: LOAD s :: _ -> replace 2 [LDG (s, x)]
    | GLOBAL x :: STORE s :: _ -> replace 2 [STG (s, x)]

      (* Small results *)
    | CALL (n, (CharT|BoolT|ByteT|ShortT)) :: _ -> 
	replace 1 [CALL (n, IntT)]
    | RETURN (CharT|BoolT|ByteT|ShortT) :: _ -> replace 1 [RETURN IntT]

      (* Eliminate operands that don't fit in 16 bits *)
    | LOCAL n :: _ when not (fits 16 n) ->
	replace 1 [LOCAL 0; CONST (integer n); BINOP (PtrT, PlusA)]
    | LDL (s, n) :: _ when not (fits 16 n) ->
	replace 1 [LOCAL n; LOAD s]
    | INCL n :: _ when not (fits 16 n) ->
	replace 1 [LDL (IntT, n); MONOP (IntT, Inc); STL (IntT, n)]
    | DECL n :: _ when not (fits 16 n) ->
	replace 1 [LDL (IntT, n); MONOP (IntT, Dec); STL (IntT, n)]
    | STL (s, n) :: _ when not (fits 16 n) ->
	replace 1 [LOCAL n; STORE s]
    | LDNW n :: _ when not (fits 16 n) ->
	replace 1 [CONST (integer n); BINOP (PtrT, PlusA); LOAD IntT]
    | STNW n :: _ when not (fits 16 n) ->
	replace 1 [CONST (integer n); BINOP (PtrT, PlusA); STORE IntT]
    | LDEW n :: _ when not (fits 16 n) ->
	replace 1 [LDL (IntT, -4); LDNW n]
    | STEW n :: _ when not (fits 16 n) ->
	replace 1 [LDL (IntT, -4); STNW n]

      (* Delete NOP *)
    | NOP :: _ -> replace 1 []

    | _ -> ()

let optstep rules changed code =
  let ch = ref true in
  let replace n c = 
    changed := true; ch := true;
    if !Config.debug > 1 then
      printf "! $ --> $\n" [fList(fInst) (Util.take n !code); fList(fInst) c];
    List.iter (do_refs decr) (Util.take n !code);
    List.iter (do_refs incr) c; 
    code := c @ Util.drop n !code in
  while !ch do
    ch := false; rules replace !code
  done

let optimize rules prog =
  let buf1 = ref prog and buf2 = ref [] in
  let changed = ref true in
  while !changed do
    changed := false;
    while !buf1 <> [] do
      optstep rules changed buf1;
      if !buf1 <> [] then begin
	buf2 := List.hd !buf1 :: !buf2;
	buf1 := List.tl !buf1
      end
    done;
    buf1 := List.rev !buf2;
    buf2 := []
  done;
  !buf1

let put_line n =
  printf "! $\n" [fStr (source_line n)];
  if !Config.linecount then printf "$\n" [fInst (LINE n)]

let put =
  function
      LINE n ->
	put_line n
    | JCASE labs ->
	printf "$\n" [fInst (JCASE labs)];
	List.iter (function x -> printf "CASEL $\n" [fLab x]) labs
    | i -> 
	printf "$\n" [fInst i]

let buf = ref []

(* gen -- buffer an instruction *)
let gen i =
  if !Config.debug > 2 then printf "!! $\n" [fInst i];
  do_refs incr i;
  buf := i :: !buf

let reduce () =
  (* Rule sets 1 and 2 do optional optimization, but rule set 3 is 
     obligatory, because it deals with large constants, which are not
     supported by the abstract machine *)
  buf := 
    optimize ruleset3
      (if !Config.optflag then 
          optimize ruleset2 (optimize ruleset1 (List.rev !buf))
        else 
          List.rev !buf)

let flush () =
  List.iter (function i -> put (rename_labs i)) !buf;
  buf := []; Hashtbl.clear label_tab
