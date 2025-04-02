open! Core

module Game : sig
  type t

  val create : string -> int -> t
  val to_string_pretty : t -> string
  val register_guess : t -> char -> t Or_error.t
end
