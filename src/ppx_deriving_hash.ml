
(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html *)

open Ppxlib
module A = Ast_helper
module B = Ast_builder.Default
let spf = Printf.sprintf

(* TODO *)

let hasher_name_of_ty_name (ty_name:string) : string =
  if ty_name = "t" then "hasher" else "hasher_" ^ ty_name

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

  | _ ->
    [%expr assert false (* TODO: hasher of type *)]

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
          | Pcstr_record _r ->
            A.Pat.construct (lid_of_str cname) (Some (A.Pat.any ())),
            [%expr [%error "cannot handle inline records yet"]] (* FIXME *)
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

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  let hasher_decls =
    List.map
      (fun ty ->
         let loc = ty.ptype_loc in
         let fname = hasher_name_of_ty ty in
         let def = hash_expr_of_tydecl ty in
         A.Vb.mk (A.Pat.var {loc;txt=fname}) def)
      type_declarations in
  let hashers = A.Str.value rec_flag hasher_decls in
  let hash_l =
    List.map (fun ty ->
        let loc = ty.ptype_loc in
        let hasher =
          A.Exp.ident (lid ~loc (hasher_name_of_ty ty))
        in
        A.Vb.mk (A.Pat.var {loc;txt=hash_name_of_ty ty})
          [%expr fun ~algo x ->
            Ppx_deriving_hash_runtime.hash ~algo ~hash:[%e hasher] x])
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

let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  let values =
    List.map
      (fun ty ->
         let loc = ty.ptype_loc in
         let name_hasher = hasher_name_of_ty ty in
         let tye = A.Typ.constr ~loc (lid_of_str ty.ptype_name) [] in
         let decl_hasher =
           A.Val.mk {loc;txt=name_hasher}
             [%type: ([%t tye]) Ppx_deriving_hash_runtime.hasher]
         in
         let name_hash = hash_name_of_ty ty in
         let decl_hash =
           A.Val.mk {loc;txt=name_hash}
             [%type: algo:(_, 'out) Ppx_deriving_hash_runtime.hash_algo ->
               [%t tye] -> 'out
             ]
         in
         [decl_hasher; decl_hash])
      type_declarations
    |> List.flatten
  in
  List.map (fun s -> A.Sig.value s) values

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf


let myderiver =
  Deriving.add "hash"
    ~sig_type_decl:intf_generator
    ~str_type_decl:impl_generator
