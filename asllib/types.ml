(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open AST
open ASTUtils
open Infix
module SEnv = StaticEnv

type env = SEnv.env

module TypingRule = Instrumentation.TypingRule

let ( |: ) = Instrumentation.TypingNoInstr.use_with

let undefined_identifier pos x =
  Error.fatal_from pos (Error.UndefinedIdentifier x)

let thing_equal astutil_equal env = astutil_equal (StaticModel.equal_in_env env)
let expr_equal = thing_equal expr_equal
let type_equal = thing_equal type_equal
let array_length_equal = thing_equal array_length_equal
let bitwidth_equal = thing_equal bitwidth_equal
let slices_equal = thing_equal slices_equal
let bitfield_equal = thing_equal bitfield_equal
let constraint_equal = thing_equal constraint_equal
let constraints_equal = thing_equal constraints_equal
let assoc_map map li = List.map (fun (x, y) -> (x, map y)) li

(* --------------------------------------------------------------------------*)

(* Begin Anonymize *)
let rec make_anonymous (env : env) (ty : ty) : ty =
  match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | Some (t1, _) -> make_anonymous env t1
      | None -> undefined_identifier ty x)
  | _ -> ty
(* End *)

(* TODO: rethink to have physical equality when structural equality? *)
(* TODO: memoize? *)
(* Begin Structure *)
let rec get_structure (env : env) (ty : ty) : ty =
  let () =
    if false then Format.eprintf "@[Getting structure of %a.@]@." PP.pp_ty ty
  in
  let with_pos = add_pos_from ty in
  (match ty.desc with
  | T_Named x -> (
      match IMap.find_opt x env.global.declared_types with
      | None -> undefined_identifier ty x
      | Some (t1, _) -> get_structure env t1)
  | T_Int _ | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ -> ty
  | T_Tuple tys -> T_Tuple (List.map (get_structure env) tys) |> with_pos
  | T_Array (e, t) -> T_Array (e, (get_structure env) t) |> with_pos
  | T_Collection fields ->
      let fields' = assoc_map (get_structure env) fields |> canonical_fields in
      T_Collection fields' |> with_pos
  | T_Record fields ->
      let fields' = assoc_map (get_structure env) fields |> canonical_fields in
      T_Record fields' |> with_pos
  | T_Exception fields ->
      let fields' = assoc_map (get_structure env) fields |> canonical_fields in
      T_Exception fields' |> with_pos)
  |: TypingRule.Structure
(* End *)

(* --------------------------------------------------------------------------*)

(* Begin BuiltinSingular *)
let is_builtin_singular ty =
  (match ty.desc with
  | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ | T_Int _ -> true
  | T_Tuple _
  | T_Array (_, _)
  | T_Collection _ | T_Record _ | T_Exception _ | T_Named _ ->
      false)
  |: TypingRule.BuiltinSingularType
(* End *)

(* Begin BuiltinAggregate *)
let is_builtin_aggregate ty =
  (match ty.desc with
  | T_Tuple _ | T_Array _ | T_Collection _ | T_Record _ | T_Exception _ -> true
  | T_Int _ | T_Bits (_, _) | T_Real | T_String | T_Bool | T_Enum _ | T_Named _
    ->
      false)
  |: TypingRule.BuiltinAggregateType
(* End *)

(* Begin BuiltinSingularOrAggregate *)
let is_builtin ty =
  (is_builtin_singular ty || is_builtin_aggregate ty)
  |: TypingRule.BuiltinSingularOrAggregate
(* End *)

(* Begin Named *)
let is_named ty =
  (match ty.desc with T_Named _ -> true | _ -> false) |: TypingRule.NamedType
(* End *)

(* Begin Anonymous *)
let is_anonymous ty = (not (is_named ty)) |: TypingRule.AnonymousType
(* End *)

(* A named type is singular if its underlying (a.k.a. anonimized) type is a builtin-singular type,
   otherwise it is aggregate. *)
(* Begin Singular *)
let is_singular env ty =
  make_anonymous env ty |> is_builtin_singular |: TypingRule.SingularType
(* End *)

(* A named type is singular if its underlying (a.k.a. anonimized) type is a builtin-aggregate type. *)
(* Begin Aggregate *)
let is_aggregate env ty =
  make_anonymous env ty |> is_builtin_aggregate |: TypingRule.AggregateType
(* End *)

(* Begin NonPrimitive *)
let rec is_non_primitive ty =
  (match ty.desc with
  | T_Real | T_String | T_Bool | T_Bits _ | T_Enum _ | T_Int _ -> false
  | T_Named _ -> true
  | T_Tuple tys -> List.exists is_non_primitive tys
  | T_Array (_, ty) -> is_non_primitive ty
  | T_Record fields | T_Exception fields | T_Collection fields ->
      List.exists (fun (_, ty) -> is_non_primitive ty) fields)
  |: TypingRule.NonPrimitiveType
(* End *)

(* Begin Primitive *)
let is_primitive ty = (not (is_non_primitive ty)) |: TypingRule.PrimitiveType
(* End *)

let parameterized_constraints =
  let next_uid = ref 0 in
  fun var ->
    let uid = !next_uid in
    incr next_uid;
    Parameterized (uid, var)

let parameterized_ty var =
  T_Int (parameterized_constraints var) |> add_dummy_annotation

let to_well_constrained ty =
  match ty.desc with
  | T_Int (Parameterized (_uid, var)) -> var_ var |> integer_exact
  | _ -> ty

let get_well_constrained_structure env ty =
  get_structure env ty |> to_well_constrained

(* --------------------------------------------------------------------------*)

module Domain = struct
  module IntSet = Diet.Z

  type syntax = AST.int_constraint list

  (** Represents the domain of an integer expression. *)
  type t = Finite of IntSet.t | Top | FromSyntax of syntax

  let add_interval_to_intset acc bot top =
    if bot > top then acc
    else
      let interval = IntSet.Interval.make bot top in
      IntSet.add interval acc

  let pp f =
    let open Format in
    function
    | Top -> pp_print_string f "ℤ"
    | Finite set -> fprintf f "@[{@,%a}@]" IntSet.pp set
    | FromSyntax slices -> PP.pp_int_constraints f slices

  exception StaticEvaluationTop

  (* Begin NormalizeToInt *)
  let eval (env : env) (e : expr) =
    match StaticModel.reduce_to_z_opt env e with
    | None -> raise StaticEvaluationTop
    | Some i -> i
  (* End *)

  (* Begin ConstraintToIntSet *)
  let add_constraint_to_intset env acc = function
    | Constraint_Exact e ->
        let v = eval env e in
        add_interval_to_intset acc v v
    | Constraint_Range (bot, top) ->
        let bot = eval env bot and top = eval env top in
        add_interval_to_intset acc bot top
  (* End *)

  (* Begin IntSetOfIntConstraints *)
  let int_set_of_int_constraints env constraints =
    match constraints with
    | [] -> Error.fatal_from ASTUtils.dummy_annotated Error.EmptyConstraints
    | _ -> (
        try
          Finite
            (List.fold_left
               (add_constraint_to_intset env)
               IntSet.empty constraints)
        with StaticEvaluationTop -> FromSyntax constraints)
  (* End *)

  (* Begin IntSetToIntConstraints *)
  let int_set_to_int_constraints =
    let interval_to_constraint interval =
      let x = IntSet.Interval.x interval and y = IntSet.Interval.y interval in
      let expr_of_z z = L_Int z |> literal in
      Constraint_Range (expr_of_z x, expr_of_z y)
    in
    fun is ->
      IntSet.fold
        (fun interval acc -> interval_to_constraint interval :: acc)
        is []
  (* End *)

  (* Begin IntSetOp *)
  let rec int_set_raise_interval_op fop op is1 is2 =
    match (is1, is2) with
    | Top, _ | _, Top -> Top
    | Finite is1, Finite is2 -> (
        try
          Finite
            (IntSet.fold
               (fun i1 -> IntSet.fold (fun i2 -> IntSet.add (fop i1 i2)) is2)
               is1 IntSet.empty)
        with StaticEvaluationTop ->
          let s1 = int_set_to_int_constraints is1
          and s2 = int_set_to_int_constraints is2 in
          int_set_raise_interval_op fop op (FromSyntax s1) (FromSyntax s2))
    | Finite is1, FromSyntax _ ->
        let s1 = int_set_to_int_constraints is1 in
        int_set_raise_interval_op fop op (FromSyntax s1) is2
    | FromSyntax _, Finite is2 ->
        let s2 = int_set_to_int_constraints is2 in
        int_set_raise_interval_op fop op is1 (FromSyntax s2)
    | FromSyntax s1, FromSyntax s2 ->
        FromSyntax (StaticOperations.constraint_binop op s1 s2)

  (* End *)

  let monotone_interval_op op i1 i2 =
    let open IntSet.Interval in
    let x = op (x i1) (x i2) and y = op (y i1) (y i2) in
    if x < y then make x y else raise StaticEvaluationTop

  let anti_monotone_interval_op op i1 i2 =
    let open IntSet.Interval in
    let x = op (x i1) (y i2) and y = op (y i1) (x i2) in
    if x < y then make x y else raise StaticEvaluationTop

  let of_literal = function
    | L_Int n -> Finite (IntSet.singleton n)
    | _ -> raise StaticEvaluationTop

  (* [of_expr env e] returns the symbolic integer domain for the integer-typed expression [e]. *)
  let rec of_expr env e =
    match e.desc with
    | E_Literal v -> of_literal v
    | E_Var x -> (
        try SEnv.lookup_constants env x |> of_literal
        with Not_found -> (
          try SEnv.type_of env x |> of_type env
          with Not_found -> Error.fatal_from e (Error.UndefinedIdentifier x)))
    | E_Unop (NEG, e1) ->
        of_expr env (E_Binop (`MINUS, !$0, e1) |> add_pos_from e)
    | E_Binop (((`PLUS | `MINUS | `MUL) as op), e1, e2) ->
        let is1 = of_expr env e1
        and is2 = of_expr env e2
        and fop =
          match op with
          | `PLUS -> monotone_interval_op Z.add
          | `MINUS -> anti_monotone_interval_op Z.sub
          | `MUL -> monotone_interval_op Z.mul
        in
        int_set_raise_interval_op fop op is1 is2
    | _ ->
        let () =
          if false then
            Format.eprintf "@[<2>Cannot interpret as int set:@ @[%a@]@]@."
              PP.pp_expr e
        in
        FromSyntax [ Constraint_Exact e ]

  and of_width_expr env e =
    let e_domain = of_expr env e in
    let exact_domain = FromSyntax [ Constraint_Exact e ] in
    match e_domain with
    | Finite int_set ->
        if Z.equal (IntSet.cardinal int_set) Z.one then e_domain
        else exact_domain
    | FromSyntax [ Constraint_Exact _ ] -> e_domain
    | _ -> exact_domain

  and of_type env ty =
    let ty = make_anonymous env ty in
    match ty.desc with
    | T_Int UnConstrained -> Top
    | T_Int (Parameterized (_uid, var)) ->
        FromSyntax [ Constraint_Exact (var_ var) ]
    | T_Int (WellConstrained (constraints, _)) ->
        int_set_of_int_constraints env constraints
    | T_Int PendingConstrained -> assert false
    | T_Bool | T_String | T_Real ->
        failwith "Unimplemented: domain of primitive type"
    | T_Bits _ | T_Enum _ | T_Array _ | T_Collection _ | T_Exception _
    | T_Record _ | T_Tuple _ ->
        failwith "Unimplemented: domain of a non singular type."
    | T_Named _ -> assert false (* make anonymous *)

  let mem v d =
    match (v, d) with
    | L_Bool _, _
    | L_Real _, _
    | L_String _, _
    | L_BitVector _, _
    | L_Label _, _ ->
        false
    | L_Int _, Top -> true
    | L_Int i, Finite intset -> IntSet.mem i intset
    | L_Int _, _ -> false

  let equal d1 d2 =
    match (d1, d2) with
    | Top, Top -> true
    | Finite is1, Finite is2 -> IntSet.equal is1 is2
    | _ -> false

  let compare _d1 _d2 = assert false

  (** The [StaticApprox] module creates constant approximation of integer
      constraints as sets of integers. *)
  module StaticApprox = struct
    (** The two possible types of approximations. *)
    type approx = Over | Under

    exception CannotOverApproximate
    (** Raised if over approximation is not possible. *)

    (** Return bottom for Under approximation, top for over approximation. *)
    let bottom_top approx =
      if approx = Over then raise CannotOverApproximate else IntSet.empty

    let make_interval approx z1 z2 =
      if Z.leq z1 z2 then IntSet.(add (Interval.make z1 z2) empty)
      else bottom_top approx

    let literal_to_z = function L_Int z -> Some z | _ -> None

    let apply_unop loc op z =
      let open Error in
      try Operations.unop_values loc Error.Static op (L_Int z) |> literal_to_z
      with ASLException { desc = UnsupportedUnop _ } -> None

    let apply_binop loc op z1 z2 =
      let open Error in
      try
        Operations.binop_values loc Static op (L_Int z1) (L_Int z2)
        |> literal_to_z
      with ASLException { desc = UnsupportedBinop _ } -> None

    (* Begin ApproxExpr *)
    let rec approx_expr approx env e =
      match e.desc with
      | E_Literal (L_Int z) -> IntSet.singleton z
      | E_Literal _ -> bottom_top approx
      | E_Var x -> (
          match approx with
          | Over -> approx_type Over env (SEnv.type_of env x)
          | Under -> IntSet.empty)
      | E_Unop (op, e') ->
          IntSet.filter_map_individual (apply_unop e op)
            (approx_expr approx env e')
      | E_Binop (op, e1, e2) ->
          IntSet.cross_filter_map_individual (apply_binop e op)
            (approx_expr approx env e1)
            (approx_expr approx env e2)
      | E_Cond (_econd, e2, e3) -> (
          let s2 = approx_expr approx env e2
          and s3 = approx_expr approx env e3 in
          match approx with
          | Over -> IntSet.union s2 s3
          | Under -> IntSet.inter s2 s3)
      | _ -> bottom_top approx
    (* End *)

    (* Begin ApproxType *)
    and approx_type approx env t =
      match t.desc with
      | T_Named _ -> make_anonymous env t |> approx_type approx env
      | T_Int (WellConstrained (cs, _)) -> approx_constraints approx env cs
      | _ -> bottom_top approx
    (* End *)

    (* Begin ApproxConstraints *)
    and approx_constraints approx env cs =
      let join =
        let empty = IntSet.empty (* will not be used *) in
        match approx with
        | Under -> list_iterated_op ~empty IntSet.inter
        | Over -> list_iterated_op ~empty IntSet.union
      in
      List.map (approx_constraint approx env) cs
      |> join |: TypingRule.ApproxConstraints
    (* End *)

    (* Begin ApproxConstraint *)
    and approx_constraint approx env = function
      | Constraint_Exact e ->
          approx_expr approx env e |: TypingRule.ApproxConstraint
      | Constraint_Range (e1, e2) -> (
          try
            let z1, z2 =
              match approx with
              | Over -> (approx_expr_min env e1, approx_expr_max env e2)
              | Under -> (approx_expr_max env e1, approx_expr_min env e2)
            in
            make_interval approx z1 z2
          with Not_found | CannotOverApproximate -> bottom_top approx)
    (* end *)

    (* Begin ApproxExprMin *)
    and approx_expr_min env e = approx_expr Over env e |> IntSet.min_elt
    (* End *)

    (* Begin ApproxExprMax *)
    and approx_expr_max env e = approx_expr Over env e |> IntSet.max_elt
    (* End *)
  end

  (* Begin SymDomIsSubset *)
  let is_subset env is1 is2 =
    let () =
      if false then Format.eprintf "Is %a a subset of %a?@." pp is1 pp is2
    in
    let open StaticApprox in
    (match (is1, is2) with
    | _, Top -> true
    | Top, _ -> false
    | Finite ints1, Finite ints2 -> IntSet.subset ints1 ints2
    | FromSyntax cs1, FromSyntax cs2 -> (
        constraints_equal env cs1 cs2
        || Fun.flip List.for_all cs1 @@ fun c1 ->
           Fun.flip List.exists cs2 @@ fun c2 ->
           constraint_equal env c1 c2
           ||
           try
             let s1 = approx_constraint Over env c1
             and s2 = approx_constraint Under env c2 in
             IntSet.subset s1 s2
           with CannotOverApproximate -> false)
    | Finite s1, FromSyntax cs2 ->
        let s2 = approx_constraints Under env cs2 in
        IntSet.subset s1 s2
    | FromSyntax cs1, Finite s2 -> (
        try
          let s1 = approx_constraints Over env cs1 in
          IntSet.subset s1 s2
        with CannotOverApproximate -> false))
    |: TypingRule.SymDomIsSubset
  (* End *)
end

(* --------------------------------------------------------------------------*)

let is_bits_width_fixed env ty =
  match ty.desc with
  | T_Bits _ -> (
      let open Domain in
      match of_type env ty with
      | Finite int_set -> IntSet.cardinal int_set = Z.one
      | Top -> false
      | _ -> failwith "Wrong domain for a bitwidth.")
  | _ -> failwith "Wrong type for some bits."

let _is_bits_width_constrained env ty = not (is_bits_width_fixed env ty)

(* --------------------------------------------------------------------------*)

(* Begin Subtype *)
let rec subtypes_names env s1 s2 =
  if String.equal s1 s2 then true
  else
    match IMap.find_opt s1 env.SEnv.global.subtypes with
    | None -> false
    | Some s1' -> subtypes_names env s1' s2

let subtypes env t1 t2 =
  (match (t1.desc, t2.desc) with
  | T_Named s1, T_Named s2 -> subtypes_names env s1 s2
  | _ -> false)
  |: TypingRule.Subtype
(* End Subtype *)

let rec bitfields_included env bfs1 bfs2 =
  let rec mem_bfs bfs2 bf1 =
    match find_bitfield_opt (bitfield_get_name bf1) bfs2 with
    | None -> false
    | Some (BitField_Simple _ as bf2) -> bitfield_equal env bf1 bf2
    | Some (BitField_Nested (name2, slices2, bfs2') as bf2) -> (
        match bf1 with
        | BitField_Simple _ -> bitfield_equal env bf1 bf2
        | BitField_Nested (name1, slices1, bfs1) ->
            String.equal name1 name2
            && slices_equal env slices1 slices2
            && incl_bfs bfs1 bfs2'
        | BitField_Type _ -> false)
    | Some (BitField_Type (name2, slices2, ty2) as bf2) -> (
        match bf1 with
        | BitField_Simple _ -> bitfield_equal env bf1 bf2
        | BitField_Nested _ -> false
        | BitField_Type (name1, slices1, ty1) ->
            String.equal name1 name2
            && slices_equal env slices1 slices2
            && subtype_satisfies env ty1 ty2)
  and incl_bfs bfs1 bfs2 = List.for_all (mem_bfs bfs2) bfs1 in
  incl_bfs bfs1 bfs2

(* Begin TypingRule.SubtypeSatisfaction *)
and subtype_satisfies env t s =
  (* A type T subtype-satisfies type S if and only if all of the following
     conditions hold: *)
  (match ((make_anonymous env s).desc, (make_anonymous env t).desc) with
  (* If S has the structure of an integer type then T must have the structure
     of an integer type. *)
  | T_Int _, T_Int _ ->
      let d_s = Domain.of_type env s and d_t = Domain.of_type env t in
      let () =
        if false then
          Format.eprintf "domain_subtype_satisfies: %a included in %a?@."
            Domain.pp d_t Domain.pp d_s
      in
      Domain.is_subset env d_t d_s
  (* If S has the structure of a real/string/bool then T must have the
     same structure. *)
  | ( ((T_Real | T_String | T_Bool) as s_anon),
      ((T_Real | T_String | T_Bool) as t_anon) ) ->
      s_anon = t_anon
  (* If S has the structure of an enumeration type then T must have
     the structure of an enumeration type with exactly the same
     enumeration literals. *)
  | T_Enum li_s, T_Enum li_t -> list_equal String.equal li_s li_t
  (*
      • If S has the structure of a bitvector type then T must have the
        structure of a bitvector type of the same width.
      • If S has the structure of a bitvector type which has bitfields then T
        must have the structure of a bitvector type of the same width and for
        every bitfield in S there must be a bitfield in T of the same name, width
        and offset, whose type type-satisfies the bitfield in S.
    *)
  | T_Bits (w_s, bf_s), T_Bits (w_t, bf_t) ->
      let bitfields_subtype = bitfields_included env bf_s bf_t in
      let widths_subtype =
        let t_width_domain = Domain.of_width_expr env w_t
        and s_width_domain = Domain.of_width_expr env w_s in
        let () =
          if false then
            Format.eprintf "Is %a included in %a?@." Domain.pp t_width_domain
              Domain.pp s_width_domain
        in
        Domain.is_subset env t_width_domain s_width_domain
      in
      bitfields_subtype && widths_subtype
  (* If S has the structure of an array type with elements of type E then
     T must have the structure of an array type with elements of type E,
     and T must have the same element indices as S. *)
  | T_Array (length_s, ty_s), T_Array (length_t, ty_t) -> (
      type_equal env ty_s ty_t
      &&
      match (length_s, length_t) with
      | ArrayLength_Expr length_expr_s, ArrayLength_Expr length_expr_t ->
          expr_equal env length_expr_s length_expr_t
      | ArrayLength_Enum (name_s, _), ArrayLength_Enum (name_t, _) ->
          String.equal name_s name_t
      | ArrayLength_Enum (_, _), ArrayLength_Expr _
      | ArrayLength_Expr _, ArrayLength_Enum (_, _) ->
          false)
  (* If S has the structure of a tuple type then T must have the
     structure of a tuple type with same number of elements as S,
     and each element in T must type-satisfy the corresponding
     element in S.*)
  | T_Tuple li_s, T_Tuple li_t ->
      List.compare_lengths li_s li_t = 0
      && List.for_all2 (type_satisfies env) li_t li_s
  (* If S has the structure of an exception type then T must have the
     structure of an exception type with at least the same fields
     (each with the same type) as S.
     If S has the structure of a record type then T must have the
     structure of a record type with at least the same fields
     (each with the same type) as S. *)
  | T_Collection fields_s, T_Collection fields_t
  | T_Exception fields_s, T_Exception fields_t
  | T_Record fields_s, T_Record fields_t ->
      List.for_all
        (fun (name_s, ty_s) ->
          List.exists
            (fun (name_t, ty_t) ->
              String.equal name_s name_t && type_equal env ty_s ty_t)
            fields_t)
        fields_s
  | T_Named _, _ -> assert false
  | _, _ -> false)
  |: TypingRule.SubtypeSatisfaction

(* End *)
(* Begin TypeSatisfaction *)
and type_satisfies env t s =
  ((* Type T type-satisfies type S if and only if at least one of the following
      conditions holds: *)
   (* T is a subtype of S *)
   subtypes env t s
  (* T subtype-satisfies S and at least one of S or T is an anonymous type *)
  || ((is_anonymous t || is_anonymous s) && subtype_satisfies env t s)
  ||
  (* T is an anonymous bitvector with no bitfields and S has the
     structure of a bitvector (with or without bitfields) of the
     same width as T. *)
  (* Here we interpret "same width" as statically the same width *)
  match (t.desc, (get_structure env s).desc) with
  | T_Bits (width_t, []), T_Bits (width_s, _) ->
      bitwidth_equal env width_t width_s
  | _ -> false)
  |: TypingRule.TypeSatisfaction
(* End *)

(* --------------------------------------------------------------------------*)

(* Begin TypeClash *)
let rec type_clashes env t s =
  (*
   Definition VPZZ:
   A type T type-clashes with S if any of the following hold:
      • they both have the structure of integers
      • they both have the structure of reals
      • they both have the structure of strings
      • they both have the structure of enumeration types with the same
        enumeration literals
      • they both have the structure of bitvectors
      • they both have the structure of arrays whose element types
        type-clash
      • they both have the structure of tuples of the same length whose
        corresponding element types type-clash
      • S is either a subtype or a supertype of T *)
  (* We will add a rule for boolean and boolean. *)
  ((subtypes env s t || subtypes env t s)
  ||
  let s_struct = get_structure env s and t_struct = get_structure env t in
  match (s_struct.desc, t_struct.desc) with
  | T_Int _, T_Int _
  | T_Real, T_Real
  | T_String, T_String
  | T_Bits _, T_Bits _
  | T_Bool, T_Bool ->
      true
  | T_Enum li_s, T_Enum li_t -> list_equal String.equal li_s li_t
  | T_Array (_, ty_s), T_Array (_, ty_t) -> type_clashes env ty_s ty_t
  | T_Tuple li_s, T_Tuple li_t ->
      List.compare_lengths li_s li_t = 0
      && List.for_all2 (type_clashes env) li_s li_t
  | _ -> false)
  |: TypingRule.TypeClash
(* End *)

let subprogram_clashes env (f1 : func) (f2 : func) =
  (* Two subprograms clash if all of the following hold:
      • they have the same name
      • they are the same kind of subprogram
      • they have the same number of formal arguments
      • every formal argument in one type-clashes with the corresponding formal
        argument in the other

     TODO: they are the same kind of subprogram
  *)
  String.equal f1.name f2.name
  && List.compare_lengths f1.args f2.args = 0
  && List.for_all2
       (fun (_, t1) (_, t2) -> type_clashes env t1 t2)
       f1.args f2.args

(* --------------------------------------------------------------------------*)

let supertypes_set (env : env) =
  let rec aux acc x =
    let acc = ISet.add x acc in
    match IMap.find_opt x env.global.subtypes with
    | Some x' -> aux acc x'
    | None -> acc
  in
  aux ISet.empty

let find_named_lowest_common_supertype env x1 x2 =
  (* TODO: Have a better algorithm? This is in O(h * log h) because set
     insertions are in O (log h), where h is the max height of the subtype
     tree. Wikipedia says it is in O(h) generally, and it can be precomputed,
     in which case it becomes O(1). *)
  let set1 = supertypes_set env x1 in
  let rec aux x =
    if ISet.mem x set1 then Some x
    else
      match IMap.find_opt x env.global.subtypes with
      | None -> None
      | Some x' -> aux x'
  in
  aux x2

(* [unpack_options li] is [Some [x1; ... x_n]] if [li] is [[Some x1; ... Some x_n]], [None] otherwise *)
let unpack_options li =
  let exception NoneFound in
  let unpack_one = function Some elt -> elt | None -> raise NoneFound in
  try Some (List.map unpack_one li) with NoneFound -> None

(* Begin LowestCommonAncestor *)
let rec lowest_common_ancestor env s t =
  let ( let+ ) o f = Option.map f o in
  (* The lowest common ancestor of types S and T is: *)
  (match (s.desc, t.desc) with
  | _, _ when type_equal env s t ->
      (* • If S and T are the same type: S (or T). *)
      Some s
  | T_Named name_s, T_Named name_t -> (
      (* If S and T are both named types: the (unique) common supertype of S
         and T that is a subtype of all other common supertypes of S and T. *)
      match find_named_lowest_common_supertype env name_s name_t with
      | Some name -> Some (T_Named name |> add_dummy_annotation)
      | None ->
          let anon_s = make_anonymous env s and anon_t = make_anonymous env t in
          lowest_common_ancestor env anon_s anon_t)
  | _, T_Named _ | T_Named _, _ ->
      let anon_s = make_anonymous env s and anon_t = make_anonymous env t in
      if type_equal env anon_s anon_t then
        Some (match s.desc with T_Named _ -> s | _ -> t)
      else lowest_common_ancestor env anon_s anon_t
  | T_Int _, T_Int UnConstrained | T_Int UnConstrained, T_Int _ ->
      (* If either S or T is an unconstrained integer type: the unconstrained
         integer type. *)
      Some integer
  | T_Int _, T_Int (Parameterized _) | T_Int (Parameterized _), T_Int _ ->
      lowest_common_ancestor env (to_well_constrained s) (to_well_constrained t)
  | T_Int (WellConstrained (cs_s, p1)), T_Int (WellConstrained (cs_t, p2)) ->
      (* If S and T both are well-constrained integer types: the
         well-constrained integer type with domain the union of the
         domains of S and T. *)
      Some (well_constrained ~precision:(precision_join p1 p2) (cs_s @ cs_t))
  | T_Bits (e_s, _), T_Bits (e_t, _) when expr_equal env e_s e_t ->
      (* We forget the bitfields if they are not equal. *)
      Some (T_Bits (e_s, []) |> add_dummy_annotation)
  | T_Array (width_s, ty_s), T_Array (width_t, ty_t)
    when array_length_equal env width_s width_t ->
      let+ t = lowest_common_ancestor env ty_s ty_t in
      T_Array (width_s, t) |> add_dummy_annotation
  | T_Tuple li_s, T_Tuple li_t when List.compare_lengths li_s li_t = 0 ->
      (* If S and T both are tuple types with the same number of elements:
         the tuple type with the type of each element the lowest common ancestor
         of the types of the corresponding elements of S and T. *)
      let+ li =
        List.map2 (lowest_common_ancestor env) li_s li_t |> unpack_options
      in
      add_dummy_annotation (T_Tuple li)
  | _ -> None)
  |: TypingRule.LowestCommonAncestor
(* End *)
