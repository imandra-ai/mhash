
module H = Ppx_deriving_hash_runtime
module Fnv = Ppx_deriving_hash_fnv

type foo0 = {
  x: int;
}

let hasher_foo0 = {
  H.hash_into=fun hash ctx foo -> hash.H.int ctx foo.x;
}

let () =
  let h1 = H.hash ~algo:Fnv.int ~hash:hasher_foo0 {x=1} in
  let h2 = H.hash ~algo:Fnv.int ~hash:hasher_foo0 {x=2} in
  assert (h1<>h2);
  ()

type foo1 =
  | A
  | B
  [@@deriving hash]

let () =
  let h1 = H.hash ~algo:Fnv.int ~hash:hasher_foo1 A in
  let h2 = H.hash ~algo:Fnv.int ~hash:hasher_foo1 B in
  assert (h1<>h2);
  ()

let () =
  let h1 = hash_foo1 ~algo:Fnv.int A in
  let h2 = hash_foo1 ~algo:Fnv.int B in
  assert (h1<>h2);
  ()

module Foo : sig
  type t [@@deriving hash]

  val x1 : t
  val x2 : t
end = struct
  type t = {
    x: bool * string;
    y: bool option
  } [@@deriving hash]

  let x1 = {x=true, "x"; y=None}
  let x2 = {x=false, "y"; y=Some true}
end

let () =
  let h1 = Foo.hash ~algo:Fnv.int Foo.x1 in
  let h2 = Foo.hash ~algo:Fnv.int Foo.x2 in
  assert (h1<>h2);
  ()

let () =
  print_endline "OK"
