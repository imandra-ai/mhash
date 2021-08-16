
(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html *)

open Ppxlib
module A = Ast_helper
module B = Ast_builder.Default
let spf = Printf.sprintf

(* TODO *)

let hasher_name_of_ty_name (ty_name:string) : string =
  if ty_name = "t" then "hasher" else "hasher_" ^ ty_name

(* name of hasher for the type parameter [v] *)
let hasher_name_poly_var v = spf "_hash_poly_%s" v

(* name of hasher function for this type *)
let hasher_name_of_ty (ty:type_declaration) : string =
  hasher_name_of_ty_name ty.ptype_name.txt

let rec hasher_name_of_lid (lid: Longident.t) : Longident.t =
  match lid with
  | Longident.Lident name ->
    Longident.Lident (hasher_name_of_ty_name name)
  | Longident.Lapply(a,b) ->
    Longident.Lapply(a, hasher_name_of_lid b)
  | Longident.Ldot (m,a) ->
    Longident.Ldot (m, hasher_name_of_ty_name a)

(* name of root hash function *)
let hash_name_of_ty (ty:type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  if ty_name = "t" then "hash" else "hash_" ^ ty_name

let lid ~loc s = {loc;txt=Longident.Lident s}
let lid_of_str {txt;loc} = lid ~loc txt

(* list separated by [;] *)
let rec mk_seq ~loc = function
  | [] -> A.Exp.construct (lid ~loc "()") None
  | [x] -> x
  | x :: tl -> A.Exp.sequence x (mk_seq ~loc tl)

let mk_lambda ~loc args body =
  List.fold_right
    (fun arg bod ->
       [%expr fun [%p (A.Pat.var {loc;txt=arg})] -> [%e bod]])
    args body

let mk_arrow ~loc args body =
  List.fold_right
    (fun arg bod -> [%type: [%t arg] -> [%t bod]])
    args body

(* apply hasher expression to expression to be hashed *)
let apply_hasher ~loc e_hasher e : expression =
  [%expr
    let e = [%e e] in
    let h = [%e e_hasher] in
    h.Ppx_deriving_hash_runtime.hash_into algo ctx e]

(* produce an expression that hashes [e].
   In scope: [algo] and [ctx]. *)
let rec hash_expr_of_type (e:expression) ~(ty:core_type) : expression =
  let loc = ty.ptyp_loc in
  let by_hasher h = apply_hasher ~loc h e in (* just apply a hasher *)
  match ty with
  | [%type: int] -> by_hasher [%expr Ppx_deriving_hash_runtime.int]
  | [%type: int32] -> by_hasher [%expr Ppx_deriving_hash_runtime.int32]
  | [%type: int64] -> by_hasher [%expr Ppx_deriving_hash_runtime.int64]
  | [%type: nativeint] -> by_hasher [%expr Ppx_deriving_hash_runtime.nativeint]
  | [%type: string] -> by_hasher [%expr Ppx_deriving_hash_runtime.string]
  | [%type: bytes] -> by_hasher [%expr Ppx_deriving_hash_runtime.bytes]
  | [%type: bool] -> by_hasher [%expr Ppx_deriving_hash_runtime.bool]
  | [%type: unit] -> by_hasher [%expr Ppx_deriving_hash_runtime.trivial]
  | [%type: float] ->
    (* hash float as its bitwise representation (an int64) *)
    apply_hasher ~loc [%expr Ppx_deriving_hash_runtime.int64]
      [%expr Int64.bits_of_float [%e e]]

  | [%type: [%t? ty_arg0] option] ->
    [%expr
      begin match [%e e] with
        | None -> algo.Ppx_deriving_hash_runtime.bool ctx false
        | Some x ->
          algo.Ppx_deriving_hash_runtime.bool ctx true;
          [%e hash_expr_of_type [%expr x] ~ty:ty_arg0]
      end]

  | [%type: [%t? ty_arg0] list] ->
    [%expr List.iter
        (fun x -> [%e hash_expr_of_type [%expr x] ~ty:ty_arg0])
        [%e e]]

  | [%type: [%t? ty_arg0] array] ->
    [%expr Array.iter
        (fun x -> [%e hash_expr_of_type [%expr x] ~ty:ty_arg0])
        [%e e]]

  | {ptyp_desc=Ptyp_var v; ptyp_loc=loc; _} ->
    (* use hasher passed as a parameter for each polymorphic argument *)
    let hasher = A.Exp.ident @@ lid ~loc @@ hasher_name_poly_var v in
    by_hasher hasher

  | {ptyp_desc=Ptyp_constr (lid, args); ptyp_loc=loc; _} ->
    (* find hasher for this type and apply it to args, themselves hashers *)
    let h = A.Exp.ident {loc;txt=hasher_name_of_lid lid.txt} in
    let args =
      args
      |> List.map
        (fun ty ->
           let hasher =
             [%expr {Ppx_deriving_hash_runtime.hash_into=
                       fun algo ctx x -> [%e hash_expr_of_type [%expr x] ~ty]
                    }]
           in
           Nolabel, hasher)
    in
    by_hasher (if args=[] then h else A.Exp.apply h args)

  | {ptyp_desc=Ptyp_tuple args; ptyp_loc=loc; _} ->
    let hash_args =
      args
      |> List.mapi
        (fun i ty ->
           let x_i = A.Exp.ident (lid ~loc @@ spf "x_%d" i) in
           hash_expr_of_type x_i ~ty)
    in
    (* [let (x1,...,xn) = e in …] *)
    let vbs =
      let tup_pat =
        A.Pat.tuple @@
        List.mapi (fun i ty ->
            let loc = ty.ptyp_loc in
            A.Pat.var {loc;txt=spf "x_%d" i}) args
      in
      [A.Vb.mk tup_pat e]
    in
    let body = mk_seq ~loc hash_args in
    A.Exp.let_ Nonrecursive vbs body

  | {ptyp_desc=Ptyp_alias (ty,_); _} ->
    hash_expr_of_type e ~ty

  | {ptyp_desc=Ptyp_arrow _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash functions"]]

  | {ptyp_desc=Ptyp_class _ | Ptyp_object _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash objects yet"]]

  | {ptyp_desc=Ptyp_package _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash first-class modules"]]

  | {ptyp_desc=Ptyp_extension _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash type extensions"]]

  | {ptyp_desc=Ptyp_variant _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash polymorphic variants yet"]]

  | {ptyp_desc=Ptyp_any; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash values of type `_`"]]

  | {ptyp_desc=Ptyp_poly _; ptyp_loc=loc; _} ->
    [%expr [%error "Cannot hash values of this type"]]

(*
  | _ ->
    [%expr [%error "Cannot hash values of this type"]]
*)

(* produce a [hasher] for the given type declaration, based on its description *)
let hash_expr_of_tydecl (decl:type_declaration) : expression =
  let loc = decl.ptype_loc in
  let self = A.Exp.ident @@ lid ~loc "self" in

  let body =
    match decl.ptype_kind with
    | Ptype_abstract ->
      begin match decl.ptype_manifest with
        | Some ty_alias ->
          hash_expr_of_type self ~ty:ty_alias (* alias, just forward to it *)
        | None ->
          [%expr [%error "cannot derive hash for abstract type"]]
      end

    | Ptype_open -> [%expr [%error "cannot derive hash for open type"]]

    | Ptype_variant cstors ->
      let hash_cstor (i:int) {pcd_args; pcd_name=cname; pcd_loc=loc; _} : case =
        (* hash the constructor index *)
        let hash_cstor_idx =
          [%expr algo.Ppx_deriving_hash_runtime.int ctx
              [%e (A.Exp.constant (A.Const.int i))]]
        in

        let lhs, rhs =
          match pcd_args with
          | Pcstr_tuple [] ->
            let lhs = A.Pat.construct (lid_of_str cname) None
            and rhs = hash_cstor_idx in
            lhs, rhs
          | Pcstr_tuple [ty0] ->
            let lhs =
              let x0 = A.Pat.var {loc;txt="x"} in
              A.Pat.construct (lid_of_str cname) (Some x0) in
            let rhs =
              let x0 = A.Exp.ident @@ lid ~loc "x" in
              [%expr
                [%e hash_cstor_idx];
                [%e hash_expr_of_type x0 ~ty:ty0]
              ]
            in
            lhs, rhs

          | Pcstr_tuple l ->
            let lhs =
              let pat =
                l
                |> List.mapi (fun i ty ->
                    let loc = ty.ptyp_loc in
                    A.Pat.var {loc;txt=spf "x_%d" i})
                |> A.Pat.tuple
              in
              A.Pat.construct (lid_of_str cname) (Some pat) in
            let rhs =
              let hash_fields =
                l
                |> List.mapi
                  (fun i ty ->
                     let loc = ty.ptyp_loc in
                     hash_expr_of_type ~ty
                       (A.Exp.ident @@ lid ~loc @@ spf "x_%d" i))
                |> mk_seq ~loc
              in
              (* hash constructor index, then arguments *)
              [%expr [%e hash_cstor_idx]; [%e hash_fields] ]
            in
            lhs, rhs

          | Pcstr_record r ->
            (* variable for the record *)
            let pat_r = A.Pat.var {loc;txt="r"} in
            let lhs = A.Pat.construct (lid_of_str cname) (Some pat_r) in
            let rhs =
              (* variable for the inline record *)
              let var_r = A.Exp.ident (lid ~loc "r") in
              let hash_fields =
                r
                |> List.map
                  (fun {pld_name;pld_loc=_;pld_type;_} ->
                     let field = A.Exp.field var_r (lid_of_str pld_name) in
                     hash_expr_of_type field ~ty:pld_type)
                |> mk_seq ~loc
              in
              (* hash constructor index, then arguments *)
              [%expr [%e hash_cstor_idx]; [%e hash_fields] ]
            in
            lhs, rhs
        in
        B.case ~lhs ~guard:None ~rhs
      in
      let branches = List.mapi hash_cstor cstors in
      A.Exp.match_ self branches

    | Ptype_record labels ->
      let e =
        labels
        |> List.map
          (fun {pld_name=field_name; pld_type; _} ->
             let self_field =
               A.Exp.field self @@ lid_of_str field_name in
             hash_expr_of_type self_field ~ty:pld_type)
        |> mk_seq ~loc
      in
      e
  in
  [%expr {Ppx_deriving_hash_runtime.hash_into=fun algo ctx self -> [%e body]}]

exception Error_gen of Location.t * string
let error_gen ~loc e = raise (Error_gen (loc,e))

let param_names ty =
  ty.ptype_params
  |> List.map (fun (ty,_) ->
      let loc = ty.ptyp_loc in
      match ty.ptyp_desc with
      | Ptyp_var a -> a
      | Ptyp_any ->
        error_gen ~loc "Cannot derive hasher for implicit param"
      | _ -> error_gen ~loc "Cannot derive hasher for non-variable type"
    )

let generate_impl_ (rec_flag, type_declarations) =

  (* parametrize by hashers of type variables *)
  let fun_poly_hashers ~loc ty body =
    mk_lambda ~loc
      (List.map hasher_name_poly_var @@ param_names ty)
      body
  in

  (* generate hasher objects *)
  let hasher_decls =
    List.map
      (fun ty ->
         let loc = ty.ptype_loc in
         let fname = hasher_name_of_ty ty in
         let def = fun_poly_hashers ~loc ty @@ hash_expr_of_tydecl ty in
         A.Vb.mk (A.Pat.var {loc;txt=fname}) def)
      type_declarations in
  let hashers = A.Str.value rec_flag hasher_decls in

  (* generate individual hash functions that call the corresponding
     hasher *)
  let hash_l =
    List.map (fun ty ->
        let loc = ty.ptype_loc in
        let hasher =
          let h = A.Exp.ident (lid ~loc (hasher_name_of_ty ty)) in
          let args =
            param_names ty
            |> List.map (fun name ->
                Nolabel, A.Exp.ident (lid ~loc @@ hasher_name_poly_var name))
          in
          if args=[] then h else A.Exp.apply h args
        in
        let rhs =
          fun_poly_hashers ~loc ty @@
          [%expr fun ~algo x ->
            Ppx_deriving_hash_runtime.hash ~algo ~hash:[%e hasher] x]
        in
        A.Vb.mk (A.Pat.var {loc;txt=hash_name_of_ty ty}) rhs
      )
      type_declarations
    |> List.map (fun vb -> A.Str.value Nonrecursive [vb])
  in
  let bracket_warn stri =
    let loc = match type_declarations with
      | [] -> Location.none | ty::_ -> ty.ptype_loc in
    let disable = [%stri [@@@ocaml.warning "-27-39"]] in
    let enable = [%stri [@@@ocaml.warning "+27+39"]] in
    [disable; stri; enable]
  in

  bracket_warn hashers @ hash_l

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  try
    generate_impl_ (rec_flag, type_declarations)
  with Error_gen (loc,msg) ->
    (* emit an error in generated code *)
    let str0 = [%stri let() = [%error [%e A.Exp.constant (A.Const.string msg)]]] in
    [str0]

let generate_intf_ type_declarations =
  let values =
    List.map
      (fun ty ->
         let loc = ty.ptype_loc in
         let name_hasher = hasher_name_of_ty ty in
         let poly_hashers =
           param_names ty
           |> List.map
             (fun n -> [%type: ([%t (A.Typ.var n)]) Ppx_deriving_hash_runtime.hasher])
         in

         (* type expression for [ty], like [(int, bool) mypair] *)
         let tye =
           A.Typ.constr ~loc (lid_of_str ty.ptype_name)
             (List.map A.Typ.var @@ param_names ty)
         in

         (* declare hasher object *)
         let decl_hasher =
           let ty =
             mk_arrow ~loc poly_hashers @@
             [%type: ([%t tye]) Ppx_deriving_hash_runtime.hasher]
           in
           A.Val.mk {loc;txt=name_hasher} ty
         in

         (* declare top hash function *)
         let name_hash = hash_name_of_ty ty in
         let decl_hash =
           let ty =
             mk_arrow ~loc poly_hashers @@
             [%type: algo:(_, 'out) Ppx_deriving_hash_runtime.hash_algo ->
               [%t tye] -> 'out
             ]
           in
           A.Val.mk {loc;txt=name_hash} ty
         in
         [decl_hasher; decl_hash])
      type_declarations
    |> List.flatten
  in
  List.map (fun s -> A.Sig.value s) values

let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  try generate_intf_ type_declarations
  with Error_gen (loc,msg) ->
    (* emit an error in generated code *)
    let s = [%sigi: val _bad_hash :
      [%error [%e A.Exp.constant (A.Const.string msg)]]] in
    [s]

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf


let myderiver =
  Deriving.add "hash"
    ~sig_type_decl:intf_generator
    ~str_type_decl:impl_generator
