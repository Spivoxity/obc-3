(*
 * dict.ml
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
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

open Mach
open Symtab
open Error
open Print
open Eval
open Gcmap

type export = Private | ReadOnly | Visible

type docstring = Error.location option

module IdMap = Map.Make(struct
  type t = ident
  let compare = compare
end)

(* environment -- abstract type of environments *)
type environment = Env of (def list ref * def IdMap.t ref) list

(* Environments are represented as a list of mappings, one for each
   nested scope: we do this to permit dummy definitions to be added in
   a sensible way, even within WITH statements. (see test/SemError0.m
   for an example).  Each block is also kept separately as a list,
   to check for multiple declarations, and so that it can be returned
   by the top_block function.  This is used for formal parameter lists
   and lists of fields in record types.  The list is kept in reverse
   order internally, so that an element can be added in constant
   time. *)

(* def_kind -- kinds of definition *)
and def_kind = 
    ConstDef of Eval.value	(* Constant (value) *)
  | EnumDef of int		(* Enum constant (value) *)
  | StringDef 			(* String *)
  | TypeDef 			(* Type *)
  | VarDef			(* Variable *)
  | ParamDef 			(* Value parameter *)
  | CParamDef			(* Const parameter *)
  | VParamDef 			(* Var parameter *)
  | FieldDef 			(* Field of record *)
  | ProcDef 			(* Procedure *)
  | PrimDef			(* Built-in primitive *)
  | ModDef of ident * environment	
				(* Imported module *)

(* def -- definitions in environment *)
and def = 
  { d_tag: ident; 		(* Name *)
    d_module: ident;		(* Module *)
    d_kind: def_kind; 		(* Kind of object *)
    mutable d_type: otype;	(* Type *)
    d_export: export;		(* Whether exported *)
    d_loc: Error.location;	(* Location of defining occurrence *)
    d_line: int;		(* Line number of def. occ. *)
    mutable d_used: bool;	(* Whether used *)
    mutable d_lab: symbol;	(* Label for global objects *)
    d_level: int;		(* Static level *)
    mutable d_offset: int;	(* Offset from frame base *)
    mutable d_param: int;	(* Offset of original parameter *)
    mutable d_env: environment;	(* Local env of procedure *)
    d_comment: docstring;	(* Location of doc comment *)
    mutable d_map: gcmap }	(* Pointer map *)

(* otype -- Oberon types *)
and otype = 
  { t_id: int;			(* Serial number *)
    mutable t_name: ident;	(* Name *)
    mutable t_module: ident;	(* Defining module *)
    t_level: int;		(* Lexical level *)
    t_guts: type_guts;		(* The type itself *)
    t_rep: Mach.metrics;	(* Representation in object program *)
    mutable t_desc: symbol;	(* Descriptor label *)
    t_map: gcmap }		(* Pointer map *)

and type_guts = 
    BasicType of kind		(* Basic type *)
  | EnumType of int		(* Enumeration type (nelems) *)
  | PointerType of def 		(* Pointer type (target) *)
  | ArrayType of int * otype	(* Array (bound, base type) *)
  | FlexType of otype		(* Open array *)
  | RecordType of rec_data 	(* Record *)
  | ProcType of proc_data	(* Procedure *)
  | BuiltinType of builtin	(* Built-in procedure *)

(* rec_data -- data about a record type *)
and rec_data =
  { r_depth: int;		(* Extension depth *)
    r_abstract: bool;		(* Whether abstract *)
    r_parent: otype;		(* Parent type (voidtype if none) *)
    r_loc: Error.location;      (* Location of type expr *)
    r_fields: def list;		(* List of fields *)
    mutable r_methods: def list	} (* List of methods *)

(* proc_data -- data about a procedure type *)
and proc_data =
  { p_kind: proc_kind;		(* Procedure or method *)
    p_fparams: def list;	(* Formal parameters *)
    p_pcount: int;		(* Total size of parameters in words *)
    p_result: otype }		(* Result type *)

and proc_kind = Procedure | Method | AbsMeth | Body

(* builtin -- data about a built-in procedure *)
and builtin =
  { b_name: string;
    b_id: libid;
    b_nargs: int;
    b_argtypes: otype list }

(* symfile -- info from an imported module *)
type symfile =
  { y_env: environment;
    y_checksum: int;
    y_doc: docstring;
    y_fname: string }

let add_def m d = IdMap.add d.d_tag d m

let add_block b (Env vs) = 
  let m = List.fold_left add_def IdMap.empty b in
  Env ((ref (List.rev b), ref m) :: vs)

let top_block =
  function
      Env ((b, ms)::_) -> List.rev !b
    | _ -> failwith "top_block"

let pop_block =
  function
      Env (_ :: vs) -> Env vs
    | _ -> failwith "pop_block"

let new_block (Env vs) = Env ((ref [], ref IdMap.empty) :: vs)

let find_def x ds = 
  let visible d = (d.d_module = !current || d.d_export <> Private) in
  List.find (fun d -> d.d_tag = x && visible d) ds

let define env d = 
  match env with
      Env ((b, m) :: vs) ->
	begin try
	  let prev = IdMap.find d.d_tag !m in
	  (* The test is needed to allow masking of hidden imported fields *)
	  if prev.d_module = !current || prev.d_export <> Private then
	    raise Exit 
	with 
	  Not_found -> ()
	end;
	b := d :: !b; m := add_def !m d
    | _ -> failwith "define"

let rec try_all f xs =
  match xs with
      [] -> raise Not_found
    | y::ys ->
        try f y with Not_found -> try_all f ys

let lookup x (Env vs) = 
  try_all (fun (b, m) -> IdMap.find x !m) vs

let empty_env = Env []

let fEnv (Env vs) =
  let fDef d = fId d.d_tag in
  let fLayer (ds, _) = fList(fDef) !ds in
  fSeq(fLayer, " // ") vs


(* Utility functions for flex types *)

let scalar t =
  match t.t_guts with 
      (BasicType _ | EnumType _ | PointerType _ | ProcType _) -> true
    | _ -> false

let is_flex t =
  match t.t_guts with FlexType _ -> true | _ -> false

let rec flexity t =
  match t.t_guts with FlexType t1 -> flexity t1 + 1 | _ -> 0

let rec flex_base t =
  match t.t_guts with FlexType t1 -> flex_base t1 | _ -> t

let def_map d =
  match d.d_kind with
      VarDef | FieldDef ->
        shift d.d_offset d.d_type.t_map
    | ParamDef ->
	if is_flex d.d_type then
	  (* A flex parameter that is copied to dynamically allocated
	     stack space *)
	  let eltype = flex_base d.d_type in
	  flex_map d.d_offset (flexity d.d_type) 
	      eltype.t_rep.m_size eltype.t_map
	else
	  (* A scalar parameter, or an aggregate parameter that is 
	     copied to statically allocated stack space *)
	  shift d.d_offset d.d_type.t_map
    | CParamDef ->
	if scalar d.d_type then 
	  shift d.d_offset d.d_type.t_map
	else 
	  shift d.d_offset ptr_map
    | VParamDef ->
	(* VAR params all begin with the address of the target.  If they
	   refer to a heap object, then it will have its own descriptor. *)
	shift d.d_offset ptr_map
    | _ -> null_map

let local_map ds = union (List.map def_map ds)


(* Types *)

let n_types = ref 0

let desctbl = ref []

let make_desc t =
  if t.t_desc = nosym then begin
    let desc =
      if t.t_module = !current && t.t_level = 0 && t.t_name <> anon then 
	sprintf "$.$" [fId !current; fId t.t_name]
      else
	genlab () in
    t.t_desc <- desc;
    desctbl := t :: !desctbl
  end

let desc_table () = List.rev !desctbl


type typespec = type_guts * metrics * gcmap

let new_type lev (g, r, m) =
  incr n_types;
  { t_id = !n_types; t_name = anon; t_module = !current; t_level = lev;
    t_guts = g; t_rep = r; t_desc = nosym; t_map = m }

let basic_type k r s =
  let map = if k = PtrT then ptr_map else null_map in
  let t = new_type 0 (BasicType k, r, map) in
  t.t_name <- intern s; t

let sys_type k r s =
  let t = basic_type k r s in
  t.t_module <- intern "SYSTEM"; t

let voidtype = basic_type VoidT void_rep "VOID"
let numtype = basic_type NumT void_rep "integer"
let bytetype = basic_type ByteT char_rep "BYTE"
let shortint = basic_type ShortT short_rep "SHORTINT"
let inttype = basic_type IntT int_rep "INTEGER"
let longint = basic_type LongT long_rep "LONGINT"
let character = basic_type CharT char_rep "CHAR"
let boolean = basic_type BoolT bool_rep "BOOLEAN"
let realtype = basic_type FloatT float_rep "REAL"
let longreal = basic_type DoubleT double_rep "LONGREAL"
let settype = basic_type SetT int_rep "SET"
let niltype = basic_type PtrT addr_rep "NIL"
let errtype = basic_type ErrT int_rep "*errtype*"

let ptrtype = sys_type PtrT addr_rep "PTR"
let longptr = sys_type LongPtrT long_rep "LONGPTR"
let sysbyte = sys_type SysByteT char_rep "BYTE"

let basic_types = 
  [voidtype; bytetype; shortint; inttype; longint; character; boolean; 
    realtype; longreal; settype; ptrtype; longptr; sysbyte]

(* Useful allocation stuff *)

(* align -- increase offset to next multiple of alignment *)
let align alignment offset =
  let margin = !offset mod alignment in
  if margin <> 0 then offset := !offset - margin + alignment

let pointer d =
  (PointerType d, addr_rep, ptr_map)

let row n t = 
  let r = t.t_rep in
  (ArrayType (n, t), 
    { m_size = n * r.m_size; m_align = r.m_align },
    repeat n r.m_size t.t_map)

let flex t = (FlexType t, void_rep, null_map)

let strtype = new_type 0 (flex character)

let record abs parent loc size0 fields =
  let depth = 
    match parent.t_guts with RecordType r -> r.r_depth+1 | _ -> 0 in
  let newrec = { r_depth = depth; r_abstract = abs; r_parent = parent; 
		    r_loc = loc; r_fields = fields; r_methods = [] } in
  let almt =
    List.fold_left (fun m d -> max m d.d_type.t_rep.m_align) 1 fields in
  let size = ref size0 in
  align almt size;
  (RecordType newrec, 
    { m_size = !size; m_align = almt },
    local_map fields)

let proctype p = (ProcType p, addr_rep, null_map)

let bodytype =
  let p = { p_kind = Body; p_fparams = []; 
		p_pcount = 0; p_result = voidtype } in
  new_type 0 (proctype p)

(* This is used only for assignments involving string constants *)
let bound t =
  match t.t_guts with 
      ArrayType (n, _) -> n 
    | FlexType _ -> 1000000
    | _ -> failwith "bound"

let base_type t =
  match t.t_guts with 
      ArrayType (n, t1) -> t1 
    | FlexType t1 -> t1
    | PointerType d -> d.d_type
    | _ -> t

let is_record t =
  match t.t_guts with RecordType _ -> true | _ -> false

let is_abstract t =
  match t.t_guts with RecordType r -> r.r_abstract | _ -> false

let rec get_record t =
  match t.t_guts with 
      RecordType r -> r 
    | _ -> failwith "get_record"

let rec ancestors t =
  match t.t_guts with
      RecordType r -> t :: ancestors r.r_parent
    | _ -> []

let get_proc t =
  match t.t_guts with ProcType p -> p | _ -> failwith "get_proc"


(* Printing types *)

let rec bounds t =
  match t.t_guts with
    ArrayType (n, t1) -> n :: bounds t1  | _ -> []

let rec array_base t =
  match t.t_guts with
    ArrayType (n, t1) -> array_base t1 | _ -> t

let rec fOType1 (dp, t) = fExt (otype1 dp t)
and otype1 dp t prf =
  (* dp = 0 means top level; dp = 1 means nested type; 
     dp >= 2 means pointer target. *)
  let do_name () =
    if dp = 0 then begin
      prf "type " [];
      if t.t_name <> anon then 
	prf "$ = " [fQual (t.t_module, t.t_name)]
    end in
  match t.t_guts with
      BasicType _ -> 
	if dp = 0 then prf "type " [];
	prf "$" [fQual (t.t_module, t.t_name)]
    | EnumType n ->
	do_name ();
	prf "<an enumerated type>" []
    | PointerType d ->
	if dp > 2 then
	  prf "..." []
	else begin
	  do_name ();
	  prf "POINTER TO $" [fOType1 (max 2 (dp+1), d.d_type)]
	end
    | ArrayType (_, _) ->
	do_name ();
	prf "ARRAY $ OF $" 
	  [fList(fNum) (bounds t); fOType1 (max dp 1, array_base t)]
    | FlexType t1 ->
	do_name ();
	prf "ARRAY OF $" [fOType1 (max dp 1, t1)]
    | RecordType _ ->
	if dp = 0 then begin
	  if t.t_name <> anon then
	    prf "record type '$'" [fQual (t.t_module, t.t_name)]
	  else
	    prf "an anonymous record type" []
	end
	else begin
	  if t.t_name <> anon then
	    prf "$" [fQual (t.t_module, t.t_name)]
	  else
	    prf "RECORD ..." []
	end
    | ProcType _ ->
	if dp = 0 then begin
	  if t.t_name <> anon then
	    prf "procedure type '$'" [fQual (t.t_module, t.t_name)]
	  else
	    prf "type PROCEDURE ..." []
	end
	else begin
	  if t.t_name <> anon then
	    prf "$" [fQual (t.t_module, t.t_name)]
	  else
	    prf "PROCEDURE ..." []
	end
    | _ -> 
	prf "[a secret internal type]" []

let fOType t = fOType1 (0, t)


(* Type tests *)

let numtest k t =
  match t.t_guts with 
      BasicType k' -> (k' <= k || k' = ErrT)
    | _ -> false

let numeric t = numtest DoubleT t
let integral t = numtest LongT t
let floating t = numeric t && not (integral t)

let kind_of t =
  match t.t_guts with 
      BasicType k -> k
    | EnumType n -> IntT
    | (PointerType _ | ProcType _) -> PtrT
    | _ -> failwith (sprintf "kind_of ($)" [fOType t])

let join_type t1 t2 =
  if kind_of t1 >= kind_of t2 then t1 else t2

let is_basic t = match t.t_guts with BasicType _ -> true | _ -> false

let is_errtype t = (t.t_guts = BasicType ErrT)

let is_enum t =
  match t.t_guts with EnumType _ -> true | _ -> false

let is_discrete t =
  match t.t_guts with
      BasicType (LongT | IntT | ShortT | ByteT | NumT 
          | CharT | BoolT | ErrT) -> true
    | EnumType n -> true
    | _ -> false

let is_enum t =
  match t.t_guts with
      EnumType n -> true
    | _ -> false

let is_proc t =
  match t.t_guts with (ProcType _ | BuiltinType _) -> true | _ -> false

let is_array t =
  match t.t_guts with (ArrayType _ | FlexType _) -> true | _ -> false

let is_pointer t =
  match t.t_guts with PointerType _ -> true | _ -> false

(* This is used for most rules where types should match *)
let same_types t1 t2 =
  match (t1.t_guts, t2.t_guts) with
    (* errtype is considered the same as any other type: *)
      (BasicType ErrT, _) -> true
    | (_, BasicType ErrT) -> true
    (* Otherwise, use name equivalence *)
    | _ -> t1.t_module = t2.t_module && t1.t_id = t2.t_id

let is_niltype t = same_types t niltype

(* This is used to match open array parameters *)
let rec array_match t1 t2 =
  match (t1.t_guts, t2.t_guts) with
      (ArrayType (b1, u1), FlexType u2) ->
	array_match u1 u2
    | (FlexType u1, FlexType u2) ->
	array_match u1 u2
    | (_, _) ->
	same_types t1 t2

(* This is used to match argument lists of procedure types *)
let rec equal_types t1 t2 = 
  match (t1.t_guts, t2.t_guts) with
      (FlexType u1, FlexType u2) ->
	equal_types u1 u2
    | (ProcType _, ProcType _) ->
        proc_match t1 t2
    | _ -> same_types t1 t2

and proc_match t1 t2 =
  if is_errtype t1 || is_errtype t2 then
    true
  else
    (match (t1.t_guts, t2.t_guts) with
        (ProcType p1, ProcType p2) ->
          p1.p_kind = p2.p_kind
          && match_args p1.p_fparams p2.p_fparams 
          && same_types p1.p_result p2.p_result
      | (_, _) -> false)

and match_args fp1 fp2 = 
  begin try 
    let match_one (f1, f2) =
      match (f1.d_kind, f2.d_kind) with
	  (((ParamDef|CParamDef), (ParamDef|CParamDef))
	      | (VParamDef, VParamDef)) ->
	    equal_types f1.d_type f2.d_type
	| (_, _) -> false in
    List.for_all match_one (List.combine fp1 fp2) 
  with Invalid_argument _ -> 
    false
  end

(* This is used to detect confusing clashes of anonymous types *)
let approx_same u1 u2 =
  let rec compare n t1 t2 =
    if n = 0 || (t1.t_module = t2.t_module && t1.t_id = t2.t_id) then
      true
    else
      match (t1.t_guts, t2.t_guts) with
          (PointerType d1, PointerType d2) ->
            compare (n-1) d1.d_type d2.d_type
        | (ArrayType (b1, u1), ArrayType (b2, u2)) ->
            b1 = b2 && compare n u1 u2
        | (FlexType u1, FlexType u2) ->
            compare n u1 u2
        | (RecordType _, RecordType _) ->
            t1.t_name = anon && t2.t_name = anon
        | (ProcType p1, ProcType p2) ->
            proc_match t1 t2
        | _ -> false in
  compare 2 u1 u2

(* This is used for matching types in assignment and value parameters *)
let rec subtype t1 t2 = 
  if same_types t1 t2 then
    true
  else begin
    match (t1.t_guts, t2.t_guts) with
	(PointerType d1, PointerType d2) -> 
	  subtype d1.d_type d2.d_type
      | (RecordType r, _) -> 
	  r.r_depth > 0 && subtype r.r_parent t2
      | _ -> false
  end  

let is_string t =
  match t.t_guts with
      ArrayType (n, t1) -> same_types t1 character
    | FlexType t1 -> same_types t1 character
    | _ -> false

let is_address t =
  is_pointer t || is_proc t || is_niltype t || same_types t ptrtype


(* Initial environment *)

let make_def1 s x k lab t =
  { d_tag = intern_sys s; d_module = anon; d_export = x; d_kind = k;
    d_used = true; d_loc = no_loc; d_line = 0; d_type = t;
    d_lab = lab; d_level = 0; d_offset = 0; d_param = 0;
    d_comment = None; d_env = empty_env; d_map = null_map }

let make_def s k t =
  make_def1 s Visible k nosym t

let builtin name i n ats = 
  let data = { b_name = name; b_id = i; b_nargs = n; b_argtypes = ats } in
  make_def name PrimDef
    (new_type 0 (BuiltinType data, void_rep, null_map))

let libproc1 name n fps rt lab =
  let fparam (s, k, t) = make_def1 s Private k nosym t in
  let p = { p_kind = Procedure; p_fparams = List.map fparam fps;
                                p_pcount = n; p_result = rt } in
  make_def1 name Visible ProcDef lab (new_type 0 (proctype p))

let libproc name n fps rt =
  libproc1 name n fps rt name

let make_env ds =
  let env = new_block empty_env in 
  List.iter (define env) ds; env

let init_env () = 
  let defs =
    [ make_def "INTEGER" TypeDef inttype;
      make_def "SHORTINT" TypeDef shortint;
      make_def "LONGINT" TypeDef longint;
      make_def "CHAR" TypeDef character;
      make_def "BOOLEAN" TypeDef boolean;
      make_def "REAL" TypeDef realtype;
      make_def "LONGREAL" TypeDef longreal;
      make_def "SET" TypeDef settype;

      (* These builtins are translated to inline code *)
      builtin "CHR" ChrFun 1 [inttype];      
      builtin "ORD" OrdFun 1 [];
      builtin "ODD" OddFun 1 [inttype];
      builtin "NEW" NewProc (-1) [];
      builtin "LEN" LenFun (-1) [];
      builtin "INC" IncProc (-1) [];
      builtin "DEC" DecProc (-1) [];
      builtin "ABS" AbsFun 1 [];
      builtin "ASSERT" Assert (-1) [];
      builtin "INCL" InclProc 2 [settype; inttype];
      builtin "EXCL" ExclProc 2 [settype; inttype];

      builtin "LSL" LslFun 2 [inttype; inttype];
      builtin "LSR" LsrFun 2 [inttype; inttype];
      builtin "ASR" AsrFun 2 [inttype; inttype];
      builtin "ROR" RorFun 2 [inttype; inttype];
      builtin "ASH" AshFun 2 [inttype; inttype];
  
      (* These library procedures are genuine procedures *)
      libproc "HALT" 1 ["n", ParamDef, inttype] voidtype ]

    @ if !Config.ob07flag then 
      [ make_def "BYTE" TypeDef bytetype;
        builtin "FLT" FltFun 1 [inttype];
        builtin "FLOOR" Entier 1 [];
        builtin "PACK" PackProc 2 [];
        builtin "UNPK" UnpkProc 2 [] ]
    else 
      [ make_def "TRUE" (ConstDef (IntVal (integer 1))) boolean;
        make_def "FALSE" (ConstDef (IntVal (integer 0))) boolean;
        libproc "COPY" 4 
  	  ["x", ParamDef, strtype; "v", VParamDef, strtype] voidtype;
        libproc "CAP" 1 ["c", ParamDef, character] character;
        builtin "MIN" MinFun 1 [];
        builtin "MAX" MaxFun 1 [];
        builtin "SHORT" Short 1 [];
        builtin "LONG" Long 1 [];
        builtin "SIZE" SizeFun 1 [];
        builtin "ENTIER" Entier 1 [] ] in 
  make_env defs  

let sysenv () = 
  let defs =
    [ make_def "PTR" TypeDef ptrtype;
      make_def "LONGPTR" TypeDef longptr;
      make_def "BYTE" TypeDef sysbyte;
      builtin "ADR" AdrFun 1 [];
      builtin "VAL" ValFun 2 [];
      builtin "BIT" BitFun 2 [inttype; inttype];
      builtin "GET" GetProc 2 [];
      builtin "PUT" PutProc 2 [];
      libproc1 (if !Config.ob07flag then "COPY" else "MOVE") 3
          ["src", ParamDef, inttype; "dest", ParamDef, inttype; 
	    "nbytes", ParamDef, inttype] voidtype "SYSTEM.MOVE";
      libproc1 "LIBERROR" 2 ["msg", CParamDef, strtype] voidtype
          "SYSTEM.LIBERROR";
      libproc1 "GC" 0 [] voidtype "SYSTEM.GC";
      libproc1 "LOADLIB" 1 ["name", CParamDef, strtype] voidtype
          "SYSTEM.LOADLIB" ]
    @ if !Config.ob07flag then [ builtin "SIZE" SizeFun 1 [] ] else [] in
  make_env defs


(* Debugger hooks *)

let debugger = ref (fun d -> ())
