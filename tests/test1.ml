
module H = Mhash
module Fnv = Mhash_fnv

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
    y: bool option;
    z: nativeint * int32 * int64 * char;
  } [@@deriving hash]

  let x1 = {x=true, "x"; y=None; z=1n, 1l, 1L, '1'}
  let x2 = {x=false, "y"; y=Some true; z=2n,2l,2L, '2'}
end

let () =
  let h1 = Foo.hash ~algo:Fnv.int Foo.x1 in
  let h2 = Foo.hash ~algo:Fnv.int Foo.x2 in
  assert (h1<>h2);
  ()

(* ## just check that the rest compiles ## *)

type big_sum =
  | B1
  | B2 of int
  | B3 of int * bool
  | B4 of {x: int; y: string; z: bool option}
[@@deriving hash]

type not_hashable = private string

type foos = {
  all_foos: Foo.t list list;
  any_foo: Foo.t option;
  foos_ignore: (not_hashable [@nohash]);
  big_data_foo: Foo.t array;
} [@@deriving hash]

module Poly : sig
  type ('a, 'b) poly [@@deriving hash]
end = struct
  type ('a, 'b) poly = {
    p_list: 'a list;
    p_tup: ('a * 'b *
            ('b [@hasher H.trivial]) option) option;
  } [@@deriving hash]
end

let () =
  print_endline "OK"
