
(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html *)

open Ppxlib


let generate_impl ~ctxt (rec_flag, type_declarations) =
  assert false

let generate_intf ~ctxt (rec_flag, type_declarations) =
  assert false

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf


let myderiver =
  Deriving.add "hash"
    ~sig_type_decl:intf_generator
    ~str_type_decl:impl_generator
