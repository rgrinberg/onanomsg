open Core.Std
open Async.Std
open Nanomsg

module String : sig
  val send : ?pos:int -> ?len:int -> socket -> String.t -> (unit, error) Result.t Deferred.t
  val recv : ?pos:int -> ?len:int -> socket -> String.t -> (unit, error) Result.t Deferred.t
end

module Bigstring : sig
  val send : ?pos:int -> ?len:int -> socket -> Bigstring.t -> (unit, error) Result.t Deferred.t
  val recv : socket -> (Bigstring.t, error) Result.t Deferred.t
end
