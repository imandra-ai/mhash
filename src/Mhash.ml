
(** Modular Hashing interface.

    This library exposes a {!hasher} type that can be used to describe
    how to hash values of a given type, regardless of the concrete
    hash function used.

    It is inspired by
    {{: https://doc.rust-lang.org/std/hash/trait.Hash.html} Rust's Hash trait}.

    A concrete hash function can be described using {!hash_algo},
    which is similar to
    {{: https://doc.rust-lang.org/std/hash/trait.Hasher.html} Rust's Hasher trait}.

    See [Mhash_fnv] or [Mhash_sha] for concrete functions. *)

(** A hash algorithm, like murmur3, Sha1, FNV, etc.

    @param 'ctx the internal mutable context that contains the intermediate hashing
    state.
    @param 'output the result of hashing a value. It can be an integer but
    might be larger (e.g. 512 bits for SHA512).
*)
type ('ctx, 'output) hash_algo =  {
  init : unit -> 'ctx;
  (** Create a new single-use context. *)

  finalize : 'ctx -> 'output;
  (** Turn the context into a hash. *)

  bytes : 'ctx -> bytes -> unit;
  (** Hash bytes. *)

  subbytes : 'ctx -> bytes -> int -> int -> unit;
  (** Hash a slice of the bytes. *)

  string : 'ctx -> string -> unit;
  (** Hash a string *)

  substring : 'ctx -> string -> int -> int -> unit;
  (** Hash a slice of the string. *)

  bool : 'ctx -> bool -> unit;
  (** Hash a boolean. *)

  int : 'ctx -> int -> unit;
  (** Hash an integer. *)

  int32 : 'ctx -> int32 -> unit;
  (** Hash an int32. *)

  int64 : 'ctx -> int64 -> unit;
  (** Hash an int64. *)

  nativeint : 'ctx -> nativeint -> unit;
  (** Hash a native integer. *)

  char : 'ctx -> char -> unit;
  (** Hash a single byte. *)
}

type 'a hasher = {
  hash_into:
    'ctx 'out.
    ('ctx, 'out) hash_algo ->
    'ctx ->
    'a -> unit
} [@@unboxed]
(** A hash function for a type ['a].

    It updates the given [ctx] using the {!hash_algo} core functions
    and possibly other {!hasher} values.
    This works for any concrete hash function that implements {!hash_algo}. *)

let bytes = { hash_into=fun algo ctx x -> algo.bytes ctx x }

let string = { hash_into=fun algo ctx x -> algo.string ctx x }

let int = { hash_into=fun algo ctx x -> algo.int ctx x }

let nativeint = { hash_into=fun algo ctx x -> algo.nativeint ctx x }

let int32 = { hash_into=fun algo ctx x -> algo.int32 ctx x }

let int64 = { hash_into=fun algo ctx x -> algo.int64 ctx x }

let bool = { hash_into=fun algo ctx x -> algo.bool ctx x }

let char = { hash_into=fun algo ctx x -> algo.char ctx x }

(** Trivial hasher, does nothing. *)
let trivial = { hash_into=fun _ _ _ -> () }

(** Option hasher.
    It calls [h] on [x] if the input is [Some x], and also injects
    the constructor into the hash context. *)
let option h = {
  hash_into=fun algo ctx xopt -> match xopt with
    | None -> algo.bool ctx false;
    | Some x ->
      algo.bool ctx true;
      h.hash_into algo ctx x;
}

(** [map ~f h] applies hasher [h] to [f x] in order to hash [x]. *)
let map ~f h = {
  hash_into=fun algo ctx x ->
    h.hash_into algo ctx (f x)
}

(** Hash a list by applying the hasher to each element. *)
let list h = {
  hash_into=fun algo ctx l ->
    List.iter (h.hash_into algo ctx) l
}

(** Hash an array by applying the hasher to each element. *)
let array h = {
  hash_into=fun algo ctx l ->
    Array.iter (h.hash_into algo ctx) l
}

(** Hash an iterator by applying the hasher to each element. *)
let iter h = {
  hash_into=fun algo ctx iter ->
    iter (h.hash_into algo ctx);
}

(** Hash a sequence of items by applying the hasher to each element. *)
let seq s = {
  hash_into=fun algo ctx l ->
    Seq.iter (s.hash_into algo ctx) l
}

let pair h1 h2 = {
  hash_into=fun algo ctx (x,y) -> h1.hash_into algo ctx x; h2.hash_into algo ctx y
}

let tup2 = pair
let tup3 h1 h2 h3 = {
  hash_into=fun algo ctx (x1,x2,x3) ->
    h1.hash_into algo ctx x1;
    h2.hash_into algo ctx x2;
    h3.hash_into algo ctx x3;
}

let tup4 h1 h2 h3 h4 = {
  hash_into=fun algo ctx (x1,x2,x3,x4) ->
    h1.hash_into algo ctx x1;
    h2.hash_into algo ctx x2;
    h3.hash_into algo ctx x3;
    h4.hash_into algo ctx x4;
}

(** Hash a value using the given algorithm and type hasher. *)
let[@inline] hash
    (type output)
    ~(algo:_ hash_algo)
    ~(hash:_ hasher) x : output =
  let ctx = algo.init () in
  hash.hash_into algo ctx x;
  algo.finalize ctx
