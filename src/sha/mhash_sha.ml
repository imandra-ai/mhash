
module type SHA = sig
  type ctx
  type t

  val init : unit -> ctx
  val finalize : ctx -> t
  val update_string : ctx -> string -> unit
  val update_substring : ctx -> string -> int -> int -> unit
end

module Make_(S : SHA) = struct
  type t = S.t
  type state = {
    buf: bytes; (* size 8. used to have fewer copies for small values *)
    ctx: S.ctx;
  }

  (* hash slice of [self.buf] of size [len] *)
  let[@inline] update_buf_ (self:state) len =
    S.update_substring self.ctx (Bytes.unsafe_to_string self.buf) 0 len

  let ctx st = st.ctx
  let algo = {
    Mhash.
    init=(fun () -> {ctx=S.init (); buf=Bytes.create 8});
    finalize=(fun {ctx;_} -> S.finalize ctx);
    int=(fun ctx x -> S.update_string ctx.ctx (string_of_int x));
    int32=(fun ctx x ->
        Bytes.set_int32_le ctx.buf 0 x;
        update_buf_ ctx 4);
    int64=(fun ctx x ->
        Bytes.set_int64_le ctx.buf 0 x;
        update_buf_ ctx 8);
    nativeint=(fun ctx x ->
        Bytes.set_int64_le ctx.buf 0 (Int64.of_nativeint x);
        update_buf_ ctx 8);
    bool=(fun ctx x ->
        Bytes.set ctx.buf 0 (if x then '\x01' else '\x00');
        update_buf_ ctx 1);
    char=(fun ctx x ->
        Bytes.set ctx.buf 0 x;
        update_buf_ ctx 1);
    string=(fun ctx x -> S.update_string ctx.ctx x);
    substring=(fun ctx x i len -> S.update_substring ctx.ctx x i len);
    bytes=(fun ctx s -> S.update_string ctx.ctx (Bytes.unsafe_to_string s));
    subbytes=(fun ctx s i len -> S.update_substring ctx.ctx (Bytes.unsafe_to_string s) i len);
  }

end

module Sha1 = Make_(Sha1)
module Sha256 = Make_(Sha256)
module Sha512 = Make_(Sha512)
