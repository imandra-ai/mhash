

(** A hash algorithm, like murmur3, Sha1, FNV, etc. *)
module type HASH_ALGO = sig
  type ctx
  type output

  val init : unit -> ctx
  val finalize : ctx -> output

  val bytes : ctx -> bytes -> unit
  val subbytes : ctx -> bytes -> int -> int -> unit
  val string : ctx -> string -> unit
  val substring : ctx -> string -> int -> int -> unit
  val bool : ctx -> bool -> unit
  val int : ctx -> int -> unit
  val int32 : ctx -> int32 -> unit
  val int64 : ctx -> int64 -> unit
  val nativeint : ctx -> nativeint -> unit
  val char : ctx -> char -> unit
end

type 'a hashable = {
  hash_into:
    'ctx 'out.
    (module HASH_ALGO with type ctx = 'ctx and type output = 'out) ->
    'ctx ->
    'a -> unit
} [@@unboxed]
(** A hash function for a type ['a].

    It updates the given [ctx] using the {!HASH_ALGO} core functions
    and other hashable instances.
    This works for any concrete hash function that implements {!HASH_ALGO}. *)

let[@inline] hash
    (type output) (type ctx)
    ~(algo:(module HASH_ALGO with type output=output and type ctx=ctx))
    ~(hash:_ hashable) x : output =
  let module Algo = (val algo) in
  let ctx = Algo.init () in
  hash.hash_into algo ctx x;
  Algo.finalize ctx
