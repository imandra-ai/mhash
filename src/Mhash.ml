

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

type 'a hasher = {
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

(** Option hasher *)
let option h = {
  hash_into=fun algo ctx xopt -> match xopt with
    | None -> algo.bool ctx false;
    | Some x ->
      algo.bool ctx true;
      h.hash_into algo ctx x
}

(** [map ~f h] applies hasher [h] to [f x] in order to hash [x]. *)
let map ~f h = {
  hash_into=fun algo ctx x ->
    h.hash_into algo ctx (f x)
}

(** Hash a list *)
let list h = {
  hash_into=fun algo ctx l ->
    List.iter (h.hash_into algo ctx) l
}

let array h = {
  hash_into=fun algo ctx l ->
    Array.iter (h.hash_into algo ctx) l
}

(** Hash an iterator *)
let iter h = {
  hash_into=fun algo ctx iter ->
    iter (h.hash_into algo ctx);
}

(** Hash a sequence of items *)
let seq s = {
  hash_into=fun algo ctx l ->
    Seq.iter (s.hash_into algo ctx) l
}

(** Hash a value using the given algorithm and type hasher. *)
let[@inline] hash
    (type output)
    ~(algo:_ hash_algo)
    ~(hash:_ hasher) x : output =
  let ctx = algo.init () in
  hash.hash_into algo ctx x;
  algo.finalize ctx
