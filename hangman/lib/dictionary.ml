open! Core

module Dictionary : sig
  type t

  val load_from_file : string -> t
  val of_list : string list -> t
  val random_word : t -> string
end = struct
  type t = string list

  let load_from_file _ = []
  let of_list l = l

  let random_word t =
    let dictionary_length = List.length t in
    let random_index = Random.int dictionary_length in
    List.nth_exn t random_index
  ;;
end
