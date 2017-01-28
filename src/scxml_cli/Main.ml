
let hello () = print_endline "Hello!"

open Cmdliner;;

let hello_t = Term.(const hello $ const ())

let () = match Term.eval (hello_t, Term.info "statechart-scxml") with
| `Error _ -> exit 1 | _ -> exit 0
