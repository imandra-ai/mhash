
(* code adapted from Containers *)

module H = Ppx_deriving_hash_runtime

type ctx = int64 ref

let mk_ (finalize:int64 -> 'out) : (ctx, 'out) H.hash_algo =
  (* FNV hashing
     https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
  *)
  let fnv_offset_basis = 0xcbf29ce484222325L in
  let fnv_prime = 0x100000001b3L in

  let[@inline] init () : ctx = ref fnv_offset_basis in
  let[@inline] finalize (r:ctx) : 'out = finalize !r in

  let[@inline] char ctx c =
    ctx := Int64.(mul !ctx fnv_prime);
    ctx := Int64.(logxor !ctx (of_int (Char.code c))) in

  let[@inline] bool ctx b = char ctx (if b then '\x01' else '\x00') in

  let[@inline] int64 ctx n =
    for k = 0 to 7 do
      ctx := Int64.(mul !ctx fnv_prime);
      ctx := Int64.(logxor !ctx (logand (shift_left n (k * 8)) 0xffL));
    done
  in

  let int32 ctx n =
    for k = 0 to 3 do
      ctx := Int64.(mul !ctx fnv_prime);
      ctx := Int64.(logxor !ctx (of_int32 Int32.(logand (shift_left n (k * 8)) 0xffl)));
    done
  in

  let[@inline] int ctx n = int64 ctx (Int64.of_int n) in
  let[@inline] nativeint ctx n = int64 ctx (Int64.of_nativeint n) in

  let subbytes ctx (str:bytes) i len =
    if i<0 || len<0 || i+len > Bytes.length str then invalid_arg "FNV.subbytes";
    for j = i to i + len - 1 do
      let c = Bytes.get str j in
      (char[@inlined]) ctx c;
    done
  in

  let bytes ctx s = subbytes ctx s 0 (Bytes.length s) in
  let string ctx (s:string) = bytes ctx (Bytes.unsafe_of_string s) in
  let substring ctx (s:string) i len = subbytes ctx (Bytes.unsafe_of_string s) i len in
  { init; finalize;
    char; bool; int; int32; int64; nativeint;
    bytes; subbytes; string; substring; }

let int64 = mk_ (fun x->x)
let int =
  let[@inline] finalize x = Stdlib.Int64.to_int x land max_int in
  mk_ finalize
