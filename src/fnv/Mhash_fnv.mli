
(** FNV hashing *)

type ctx
(** Mutable context, holding an int64 *)

val int64 : (ctx, int64) Mhash.hash_algo
(** FNV hashing, returning an int64. *)

val int : (ctx, int) Mhash.hash_algo
(** FNV hashing, returning an int. The int is obtained from an [int64]
    and then truncated and made nonnegative. *)
