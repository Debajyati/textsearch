open Cmdliner
open Search

(* Define arguments *)
let search_term =
  let doc = "The term to search for" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TERM" ~doc)

let file_name =
  let doc = "The file to search in" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE" ~doc)

(* Core command logic *)
let run term file =
  let matches = search_in_file term file in
  print_matches matches;
  if List.length matches = 0 then
    (Printf.eprintf "No matches found.\n"; exit 1)
  else
    exit 0

(* Create the command *)
let cmd =
  let info =
    Cmd.info "textsearch"
      ~version:"0.1.0"
      ~doc:"Search for a term in a file"
  in
  Cmd.v info
    Term.(const run $ search_term $ file_name)

(* Main entry point *)
let () = Stdlib.exit (Cmd.eval cmd)

