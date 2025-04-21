open! Core
open Game
open Dictionary

let run_game () =
  let dictionary =
    Dictionary.of_list
      [ "Hello, world!"; "Hey! Who turned out the lights?"; "Massachusetts" ]
  in
  let starting_word = Dictionary.random_word dictionary in
  let game = Game.create starting_word 3 in
  let guesses = [ 'e'; 'o'; 'b'; 'h'; 'L'; 'r'; 'd' ] in
  let game_or_error =
    List.fold guesses ~init:(Ok game) ~f:(fun game_or_error c ->
      Or_error.bind game_or_error ~f:(fun game -> Game.register_guess game c))
  in
  let to_print =
    match game_or_error with
    | Ok game -> Game.to_string_pretty game
    | Error e -> Error.to_string_hum e
  in
  print_endline to_print
;;
