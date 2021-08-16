# Ppx_deriving_hash

A deriving implementation for hashing functions.

It ships with a runtime library that implements some universal hashing
interfaces, and an implementation of FNV.

## Usage

After adding `(preprocess (pps ppx_deriving_hash))` in dune, this should work:

```ocaml
# type foo = {
  x : int;
  y : string;
} [@@deriving hash] ;;
type foo = { x : int; y : string; }
val hasher_foo : foo Ppx_deriving_hash_runtime.hasher =
  {Ppx_deriving_hash_runtime.hash_into = <fun>}
val hash_foo : algo:('a, 'b) Ppx_deriving_hash_runtime.hash_algo -> foo -> 'b =
  <fun>

# #require "ppx_deriving_hash.fnv";;
# hash_foo ~algo:Ppx_deriving_hash_fnv.int {x=1; y="foo"};;
- : int = 3535931368763302282
```

### Attributes

- `[@nohash]` on a type will skip hashing for this type.
  Useful when a type declaration contains some function fields, or
  other fields that shouldn't affect the hash.
- `[@hasher <expr>]` forces the ppx to use `<expr>` as the hasher for
  the given type.
  The attribute for a type `ty` must be an expression of type `ty Ppx_deriving_hash_runtime.hasher`
