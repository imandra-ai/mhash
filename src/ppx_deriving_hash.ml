
(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html *)

open Ppxlib
module A = Ast_helper

(* TODO *)

(* name of hasher function for this type *)
let hasher_name_of_ty (ty:type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  if ty_name = "t" then "hasher" else "hasher_" ^ ty_name

(* name of root hash function *)
let hash_name_of_ty (ty:type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  if ty_name = "t" then "hash" else "hash_" ^ ty_name

(* how to hash a type *)
let rec hash_expr_of_type (_ty:core_type) : expression =
  assert false

let hash_expr_of_tydecl (decl:type_declaration) : expression =
  let loc = decl.ptype_loc in
  let body =
    match decl.ptype_kind with
    | Ptype_abstract -> [%expr [%error "cannot derive hash for abstract type"]]
    | Ptype_open -> [%expr [%error "cannot derive hash for open type"]]
    | Ptype_variant _cstors ->
      [%expr assert false] (* TODO *)
    | Ptype_record _labels ->
      [%expr assert false] (* TODO *)
  in
  [%expr {Ppx_deriving_hash_runtime.hash_into=fun hash ctx self -> [%e body]}]

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
          A.Exp.ident {loc;txt=Longident.Lident (hasher_name_of_ty ty)}
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
         let tye = A.Typ.var ~loc ty.ptype_name.txt in
         let decl_hasher =
           A.Val.mk {loc;txt=name_hasher}
             [%type: ([%t tye]) Ppx_deriving_hash_runtime.hashable]
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
