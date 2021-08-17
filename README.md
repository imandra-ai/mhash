# Mhash + `ppx_deriving_mhash`

This project contains:

- `mhash`, to implement hashing for types in a way that is compatible with many
  concrete hashing functions
- `ppx_deriving_mhash` to automatically derive such hashers

The `mhash` library comes with an implementation of
the [FNV](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function)
algorithm.

## Using the ppx

After adding `(preprocess (pps ppx_deriving_hhash))` in dune, this should work:

```ocaml
# type foo = {
  x : int;
  y : string;
} [@@deriving mhash] ;;
type foo = { x : int; y : string; }
val hasher_foo : foo Mhash.hasher =
  {Mhash.hash_into = <fun>}
val hash_foo : algo:('a, 'b) Mhash.hash_algo -> foo -> 'b =
  <fun>

# #require "mhash.fnv";;
# hash_foo ~algo:Mhash_fnv.int {x=1; y="foo"};;
- : int = 3535931368763302282
```

### Attributes

- `[@nohash]` on a type will skip hashing for this type.
  Useful when a type declaration contains some function fields, or
  other fields that shouldn't affect the hash.
- `[@hasher <expr>]` forces the ppx to use `<expr>` as the hasher for
  the given type.
  The attribute for a type `ty` must be an expression of type `ty Mhash.hasher`
