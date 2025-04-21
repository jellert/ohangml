open! Core

module Dictionary : sig
  type t

  val load_from_file : string -> t
  val of_list : string list -> t
  val random_word : t -> string
end
