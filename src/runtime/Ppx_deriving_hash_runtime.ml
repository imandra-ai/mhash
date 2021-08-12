

(** A hash algorithm, like murmur3, Sha1, FNV, etc. *)
type ('ctx, 'output) hash_algo =  {
  init : unit -> 'ctx;
  finalize : 'ctx -> 'output;

  bytes : 'ctx -> bytes -> unit;
  subbytes : 'ctx -> bytes -> int -> int -> unit;
  string : 'ctx -> string -> unit;
  substring : 'ctx -> string -> int -> int -> unit;
  bool : 'ctx -> bool -> unit;
  int : 'ctx -> int -> unit;
  int32 : 'ctx -> int32 -> unit;
  int64 : 'ctx -> int64 -> unit;
  nativeint : 'ctx -> nativeint -> unit;
  char : 'ctx -> char -> unit;
}

type 'a hashable = {
  hash_into:
    'ctx 'out.
    ('ctx, 'out) hash_algo ->
    'ctx ->
    'a -> unit
} [@@unboxed]
(** A hash function for a type ['a].

    It updates the given [ctx] using the {!HASH_ALGO} core functions
    and other hashable instances.
    This works for any concrete hash function that implements {!HASH_ALGO}. *)

let[@inline] hash
    (type output) (type ctx)
    ~(algo:_ hash_algo)
    ~(hash:_ hashable) x : output =
  let ctx = algo.init () in
  hash.hash_into algo ctx x;
  algo.finalize ctx
