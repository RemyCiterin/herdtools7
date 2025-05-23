(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
(* Memory order *)

module ScopeGen = ScopeGen.NoGen
(* Atoms *)
open Code
open MemOrder
let bellatom = false

module SIMD = NoSIMD

type atom = MemOrder.t

let default_atom = SC
let instr_atom = None

let applies_atom a d = match a,d with
| (Acq|Acq_Rel|Con),W -> false
| (Rel|Acq_Rel),R -> false
| _,_ -> true
let is_ifetch _ = false
let compare_atom = Misc.polymorphic_compare

include MachMixed.No

let pp_plain = Code.plain
let pp_as_a = Some SC
let pp_atom = pp_mem_order_short

let fold_non_mixed f k =
  let k = f Acq k in
  let k = f Rel k in
  let k = f Acq_Rel k in
  let k = f SC k in
  let k = f Rlx k in
  let k = f Con k in
  k

let fold_atom = fold_non_mixed

let worth_final _ = false

let varatom_dir _d f = f None

let merge_atoms a1 a2 = if a1=a2 then Some a1 else None

let overlap_atoms _ _ = true

let atom_to_bank _ = Code.Ord

include NoMixed
include NoWide

module PteVal = PteVal_gen.No(struct type arch_atom = atom end)

(* Fences, to be completed *)

type fence = MemOrder.t

let is_isync _ = false

let compare_fence = MemOrder.compare

let default = SC
let strong = SC

let pp_fence f = sprintf "Fence%s" (pp_mem_order_short f)


let do_fold_fence f k =
  let k = f Acq k in
  let k = f Rel k in
  let k = f Acq_Rel k in
  let k = f SC k in
  let k = f Rlx k in
  let k = f Con k in
  k

let fold_cumul_fences = do_fold_fence
let fold_all_fences =  do_fold_fence
let fold_some_fences =  do_fold_fence

let orders _f _d1 _d2 = true

let var_fence f r = f default r

(* Basic C arch *)
type arch_reg = { id:int }

let reg_compare {id=id1} {id=id2} = Misc.int_compare id1 id2

let dump_reg r = sprintf "r%i" r.id

type location =
  | Loc of string
  | Reg of Code.proc * arch_reg

let dump_loc = function
  | Loc loc -> loc
  | Reg (p,r) -> sprintf "%i:%s" p (dump_reg r)

let pp_location = dump_loc

let pp_location_brk loc = match loc with
  | Loc loc -> sprintf "[%s]" loc
  | Reg _ -> dump_loc loc


let location_compare loc1 loc2 = match loc1,loc2 with
| Loc _,Reg _ -> -1
| Reg _,Loc _ -> 1
| Loc loc1,Loc loc2 -> String.compare loc1 loc2
| Reg (p1,r1),Reg (p2,r2) -> begin match Misc.int_compare p1 p2 with
  | 0 -> reg_compare r1 r2
  | r -> r
end

let of_reg p r = Reg (p,r)
let pp_i _ = assert false
let of_loc loc = Loc (as_data loc)

type tbase = TypBase.t

let dump_tbase t = TypBase.pp t

type typ = Plain of tbase | Atomic of tbase

let is_default = function
  | Plain t|Atomic t -> TypBase.is_default t


let dump_typ = function
  | Plain t -> dump_tbase t
  | Atomic TypBase.Int -> "atomic_int"
  | Atomic t -> sprintf "_Atomic %s" (dump_tbase t)

type exp =
  | Load of location
  | AtomicLoad of MemOrder.t * exp
  | AtomicExch of MemOrder.t * exp * Code.v
  | AtomicFetchOp of MemOrder.t * exp * Code.v
  | Deref of exp
  | Const of Code.v
  | AssertVal of exp * Code.v
  | AddZero of exp * location

let addrs_of_location = function
  | Reg _ -> StringSet.empty
  | Loc loc -> StringSet.singleton loc


let rec addrs_of_exp = function
  | Const _ -> StringSet.empty
  | Load loc -> addrs_of_location loc
  | AddZero (loc1,loc2) ->
      StringSet.union
        (addrs_of_exp loc1)
        (addrs_of_location loc2)
  | Deref e|AssertVal (e,_)
  | AtomicLoad (_,e)|AtomicExch (_,e,_)|AtomicFetchOp (_,e,_)-> addrs_of_exp e

type cond = Eq | Ne

type condexp = exp * cond * exp
type ins =
  | Seq of ins * ins
  | Decl of typ * arch_reg * exp option
  | Store of exp * exp
  | SetReg of arch_reg * exp
  | AtomicStore of MemOrder.t * exp * exp
  | Fence of fence
  | Loop of ins
  | If of condexp * ins * ins option
  | Break
  | Decr of arch_reg
  | Nop

let addrs_ofcondexp (e1,_,e2) =
  StringSet.union (addrs_of_exp e1) (addrs_of_exp e2)

let rec addrs_of = function
  | Break | Fence _ | Decr _ | Nop | Decl (_,_,None) -> StringSet.empty
  | Seq (i1,i2) -> StringSet.union (addrs_of i1) (addrs_of i2)
  | Decl (_,_,Some e)
  | SetReg (_,e) -> addrs_of_exp e
  | Store (loc,e)|AtomicStore (_,loc,e) ->
      StringSet.union  (addrs_of_exp loc) (addrs_of_exp e)
  | Loop i -> addrs_of i
  | If (ce,itrue,ifalse) ->
      StringSet.union (addrs_ofcondexp ce)
        (StringSet.union (addrs_of itrue) (addrs_of_opt ifalse))
and addrs_of_opt = function
  | None -> StringSet.empty
  | Some i -> addrs_of i

let seq i1 i2 = match i1,i2 with
| (Nop,i)|(i,Nop) -> i
| _,_ -> Seq (i1,i2)

let seqs is = List.fold_right seq is Nop

let rec is_nop = function
  | Nop|Decl (_,_,None) -> true
  | Seq (i1,i2) -> is_nop i1 && is_nop i2
  | _ -> false

(* Dependencies, no CTRLISYNC *)

include CDep

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"


(* Read-Modify-Write *)
type rmw =
  | Exch
  | Add

type rmw_atom = atom

let pp_rmw compat = function
  | Exch -> if compat then "Rmw" else "Exch"
  | Add -> "Fetch.Add"

let is_one_instruction _ = true

let fold_rmw f r = let r = f Add r in  f Exch r

let fold_rmw_compat f r = f Exch r

let tr_atom_rmw omo_r omo_w = match omo_r,omo_w with
| (None,_)|(_, None) -> None
| (Some mo_r,Some mo_w) ->
    try
      Some
        (match mo_r,mo_w with
         | SC,SC -> SC
         | Rlx,Rlx -> Rlx
         | Acq,Rel -> Acq_Rel         | Acq,Rlx -> Acq
         | Rlx,Rel -> Rel
         | _,_ -> raise Exit)
    with Exit -> None

let applies_atom_rmw _ ar aw = match ar,aw with
| None,None -> true (* to allow edge lexemes `Rmw`  *)
| _,_ ->
    match tr_atom_rmw ar aw with
    | Some _ -> true
    | None -> false

let show_rmw_reg _ = true

let compute_rmw rmw old co =
  let old = Code.value_to_int old in
  let co = Code.value_to_int co in
  let new_value = match rmw with
  | Exch -> co
  | Add -> old+co in
  Code.value_of_int new_value

include NoEdge
