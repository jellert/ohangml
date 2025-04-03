open! Core
open Game

let run_game () =
  let game = Game.create "Hello, world!" 3 in
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
