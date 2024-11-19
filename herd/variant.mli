(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

type t =
  | Success     (* Riscv Model with explicit success dependency *)
  | Instr       (* Define "instr" relation, ie generated by the same
                   instruction instance *)
  | SpecialX0   (* Some events by AMO to or from x0 are not generated *)
  | NoRMW       (* No RMW event for C *)
  | AcqRelAsFence (* Riscv: Expand load acquire and store release as fences *)
  | BackCompat (* Linux, Backward compatibility -> LISA *)
  | FullScDepend    (* Complete dependencies for Store Conditinal *)
  | SplittedRMW  (* Splitted RMW events for riscv *)
  | SwitchDepScWrite     (* Switch dependency on sc mem write, riscv *)
  | SwitchDepScResult    (* Switch dependency from address read to sc result write, riscv,aarch64 *)
  | LrScDiffOk      (* Lr/Sc paired to <> addresses may succeed (!) *)
  | NotWeakPredicated (* NOT "Weak" predicated instructions, not performing non-selected events, aarch64 *)
  | LKMMVersion of [
        `lkmmv1 (* Legacy mode (wrapp rmw[Mb] instructions with explicit Mb fences, add noreturn tags) *)
      | `lkmmv2 (* Avoid wrapping rmw[Mb] instructions with explicit Mb fences and adding noreturn tags *)
    ]
 (* Mixed size *)
  | Mixed
  | Unaligned
 (* Do not check (and reject early) mixed size tests in non-mixed-size mode *)
  | DontCheckMixed
  | MemTag           (* Memory Tagging, synonym of MTE *)
  | MTEPrecision of Precision.t (* MTE tag mismatch handling *)
  | FaultHandling of Fault.Handling.t (* Fault handling *)
  | CutOff
  | Morello
  | Neon
  | SVE (* Specify SVE *)
  | SVELength of int (* vector size in bits, must be multiple of 128 *)
  | SME
  | SMELength of int (* vector size in bits, must be multiple of 128 *)
(* Branch speculation+ cat computation of dependencies *)
  | Deps
  | Instances (* Compute dependencies on instruction instances *)
 (*Replaces old KVM -> Virtual memory *)
  | VMSA
(* AArch64: Enhanced Translation Synchronization - FEAT_ETS, FEAT_ETS2, FEAT_ETS3 *)
  | ETS (*Deprecated*)
  | ETS2 (*New feature introduced after deprecating ETS*)
  | ETS3 (*A feature further strengthening ETS2*)
(* AArch64: Enhanced Exception Synchronization - FEAT_ExS *)
  | ExS | EIS | EOS
(* Do not insert branching event between pte read and accesses *)
  | NoPteBranch
(* Pte-Squared: all accesses through page table, including PT accesses *)
  | PTE2
(* Generate extra spurious updates based upon load on pte. *)
  | PhantomOnLoad
(* Optimise Rf enumeration leading to rmw *)
  | OptRfRMW
(* Allow some constrained unpredictable, behaviours.
   AArch64: LDXR / STXR of different size or address may succeed. *)
  | ConstrainedUnpredictable
(* Perform experiment *)
  | Exp
(* Instruction-fetch support (AKA "self-modifying code" mode) *)
  | Ifetch
(* CacheType features *)
  | DIC
  | IDC
(* Have cat interpreter to optimise generation of co's *)
  | CosOpt
(* Test something *)
  | Test
(* One hundred tests *)
  | T of int
(* ASL Processing *)
  (* In AArch64 arch, use ASL to interprete AArch64 instructions when possible. *)
  | ASL
  (* While interpreting ASL litmus test, include AArch64 shared pseudocode. *)
  | ASL_AArch64
  (* When using aarch ASL, use ASL version v0 or v1 *)
  | ASLVersion of [ `ASLv0 | `ASLv1 ]
(* ASL Typing control *)
  | ASLType of [`Warn|`Silence|`TypeCheck]
(* Activate ASL experimental mode *)
  | ASLExperimental
(* UDF control in ASL+AArch64 mode *)
  | ASL_AArch64_UDF
(* Signed Int128 types *)
  | S128
(* Strict interpretation of variant, e.g. -variant asl,strict *)
  | Strict
(* Semi-strict interpretation of variant, e.g. -variant asl,warn *)
  | Warn
(* Telechat variant - implements unconditional branches as exit, and any other optional quirks*)
  | Telechat
  | NV2
(* Old solver, new solver proceeds by substitution following toplogical sort *)
  | OldSolver
(* Accept cyclic equation sets as being solvable *)
  | OOTA
(* Pointer authentication code, represent the activation of the keys that we can
   find in the system register `SCTLR_EL1.En*` *)
  | Pac
(* Fault generation with Pointer authentication code *)
  | FPac
(* Allow to use pac(pac(...)) using the XOR of two pac fields *)
  | ConstPacField


val compare : t -> t -> int
val equal : t -> t -> bool
val tags : string list
val parse : string -> t option
val pp : t -> string

(* switch variant that flips an arch-dependent, default value *)
val get_default :  Archs.t -> t -> bool

(* Get value for switchable variant *)
val get_switch : Archs.t -> t -> (t -> bool) -> bool

val set_mte_precision : Precision.t ref -> t -> bool
val set_fault_handling : Fault.Handling.t ref -> t -> bool

val set_sve_length : int ref -> t -> t option
val set_sme_length : int ref -> t -> t option
val check_tag : t -> t list

