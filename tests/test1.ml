
module H = Ppx_deriving_hash_runtime
module Fnv = Ppx_deriving_hash_fnv

type foo1 =
  | A
  | B
  [@@deriving hash]

let () =
  let x = H.hash ~algo:Fnv.int ~hash:hash_foo1 A in
  let y = H.hash ~algo:Fnv.int ~hash:hash_foo1 B in
  assert (x<>y);
  ()

