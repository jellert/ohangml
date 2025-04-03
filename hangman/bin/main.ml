open! Core
open Hangman.Main

let () =
  let name : string = "jellert" in
  print_endline ("Hello, " ^ name);
  run_game ()
;;
