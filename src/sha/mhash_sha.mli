
(** Implementation of Mhash using Sha *)

module Sha1 : sig
  type state
  type t = Sha1.t

  val ctx : state -> Sha1.ctx
  val algo : (state, t) Mhash.hash_algo
end

module Sha256 : sig
  type state
  type t = Sha256.t

  val ctx : state -> Sha256.ctx
  val algo : (state, t) Mhash.hash_algo
end

module Sha512 : sig
  type state
  type t = Sha512.t

  val ctx : state -> Sha512.ctx
  val algo : (state, t) Mhash.hash_algo
end

