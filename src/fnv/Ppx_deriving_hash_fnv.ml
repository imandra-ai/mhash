
(* code adapted from Containers *)

module type HASH_ALGO = Ppx_deriving_hash_runtime.HASH_ALGO

module Make(Arg : sig
    type output
    val finalize : int64 -> output
  end)
  : HASH_ALGO with type output = Arg.output
= struct
  type output = Arg.output
  type ctx = int64 ref

  (* FNV hashing
     https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
  *)
  let fnv_offset_basis = 0xcbf29ce484222325L
  let fnv_prime = 0x100000001b3L

  let[@inline] init () : ctx = ref fnv_offset_basis
  let[@inline] finalize (r:ctx) : output = Arg.finalize !r

  let[@inline] char ctx c =
    ctx := Int64.(mul !ctx fnv_prime);
    ctx := Int64.(logxor !ctx (of_int (Char.code c)))

  let[@inline] bool ctx b = char ctx (if b then '\x01' else '\x00')

  let[@inline] int64 ctx n =
    for k = 0 to 7 do
      ctx := Int64.(mul !ctx fnv_prime);
      ctx := Int64.(logxor !ctx (logand (shift_left n (k * 8)) 0xffL));
    done

  let int32 ctx n =
    for k = 0 to 3 do
      ctx := Int64.(mul !ctx fnv_prime);
      ctx := Int64.(logxor !ctx (of_int32 Int32.(logand (shift_left n (k * 8)) 0xffl)));
    done

  let[@inline] int ctx n = int64 ctx (Int64.of_int n)
  let[@inline] nativeint ctx n = int64 ctx (Int64.of_nativeint n)

  let subbytes ctx (str:bytes) i len =
    if i<0 || len<0 || i+len > Bytes.length str then invalid_arg "FNV.subbytes";
    for j = i to i + len - 1 do
      let c = Bytes.get str j in
      (char[@inlined]) ctx c;
    done

  let bytes ctx s = subbytes ctx s 0 (Bytes.length s)
  let string ctx (s:string) = bytes ctx (Bytes.unsafe_of_string s)
  let substring ctx (s:string) i len = subbytes ctx (Bytes.unsafe_of_string s) i len
end[@@inline]

module Int64 = Make(struct
    type output = int64
    let[@inline] finalize x = x
    end)

let int64 = (module Int64 : HASH_ALGO with type output = int64)

module Int = Make(struct
    type output = int
    let[@inline] finalize x = Stdlib.Int64.to_int x land max_int
    end)

let int = (module Int : HASH_ALGO with type output = int)
