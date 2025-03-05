let concat a b = a ^ b

let () =
  let name = "Mark" in
  print_endline (concat "Hello, " (concat name "!"))
;;
