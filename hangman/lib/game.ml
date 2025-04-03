open! Core

module Game : sig
  type t

  val create : string -> int -> t
  val to_string_pretty : t -> string
  val register_guess : t -> char -> t Or_error.t
end = struct
  module Guesses = struct
    type t = char list
  end

  module Hidden_word = struct
    type t = string

    let create (word : string) : t = word
    let to_string_revealed (t : t) : string = t

    let to_string_given_guesses (t : t) (guesses : Guesses.t) : string =
      let word_as_char_list = String.to_list t in
      let hidden_word_as_list =
        List.map word_as_char_list ~f:(fun c ->
          if List.mem guesses c ~equal:Char.equal || not (Char.is_alpha c)
          then String.of_char c
          else "_")
      in
      String.concat hidden_word_as_list ~sep:" "
    ;;

    let get_bad_guesses (t : t) (guesses : Guesses.t) : Guesses.t =
      List.filter guesses ~f:(fun guess -> not (String.contains t guess))
    ;;

    let how_many_bad_guesses (t : t) (guesses : Guesses.t) : int =
      get_bad_guesses t guesses |> List.length
    ;;

    let is_revealed_with_guesses (t : t) (guesses : Guesses.t) : bool =
      String.for_all t ~f:(fun c ->
        List.exists guesses ~f:(Char.equal c) || not (Char.is_alpha c))
    ;;
  end

  module Game_state = struct
    type t =
      | In_progress
      | Loss
      | Victory
  end

  type t =
    { word : Hidden_word.t
    ; total_lives : int
    ; guesses : Guesses.t
    }

  let create word_possibly_lowercase total_lives : t =
    let word = String.uppercase word_possibly_lowercase in
    { word = Hidden_word.create word; total_lives; guesses = [] }
  ;;

  let register_guess t guess_possibly_lowercase =
    let { guesses; _ } = t in
    let guess = Char.uppercase guess_possibly_lowercase in
    if not (Char.is_alpha guess)
    then Or_error.error_string (String.of_char guess ^ " is not a letter")
    else (
      match List.mem guesses guess ~equal:Char.equal with
      | true -> Or_error.error_string ("Already guessed " ^ String.of_char guess)
      | false ->
        Or_error.return
          { t with guesses = List.sort (guess :: guesses) ~compare:Char.compare })
  ;;

  let get_lives_remaining { word; total_lives; guesses } =
    total_lives - Hidden_word.how_many_bad_guesses word guesses
  ;;

  let get_game_state t : Game_state.t =
    let { word; guesses; _ } = t in
    if get_lives_remaining t <= 0
    then Loss
    else if Hidden_word.is_revealed_with_guesses word guesses
    then Victory
    else In_progress
  ;;

  let remaining_lives_to_string t : string =
    let lives_remaining = get_lives_remaining t in
    let lives_list = List.init lives_remaining ~f:(fun _ -> "o") in
    "Lives remaining: " ^ String.concat lives_list ~sep:""
  ;;

  let previous_bad_guesses_to_string { word; guesses; _ } : string =
    let bad_guesses = Hidden_word.get_bad_guesses word guesses in
    String.concat (List.map bad_guesses ~f:String.of_char) ~sep:" "
  ;;

  let to_string_pretty t =
    let { word; guesses; _ } = t in
    let components =
      match get_game_state t with
      | Victory ->
        [ "You win!"
        ; "The word was: " ^ Hidden_word.to_string_revealed word
        ; "Your incorrect guesses: " ^ previous_bad_guesses_to_string t
        ; remaining_lives_to_string t
        ]
      | Loss ->
        [ "You lose..."
        ; "The word was: " ^ Hidden_word.to_string_revealed word
        ; "Your incorrect guesses: " ^ previous_bad_guesses_to_string t
        ]
      | In_progress ->
        [ "Incorrect guesses so far: " ^ previous_bad_guesses_to_string t
        ; remaining_lives_to_string t
        ; ""
        ; Hidden_word.to_string_given_guesses word guesses
        ]
    in
    String.concat components ~sep:"\n"
  ;;
end
