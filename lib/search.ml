let search_in_file term file =
  let ic = open_in file in
  let rec process_lines line_num acc =
    try
      let line = input_line ic in
      let new_acc =
        if String.contains line term.[0] then  (* Use the first character of the term for checking *)
          (line_num, line) :: acc
        else
          acc
      in
      process_lines (line_num + 1) new_acc
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  process_lines 1 [];;

let print_matches matches =
  List.iter
    (fun (line_num, line) ->
      Printf.printf "%d: %s\n" line_num line)
    matches;;
