let rec subsequences lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let subs = subsequences xs in
      subs @ List.map (fun sub -> x :: sub) subs

let search_in_file ~case_sensitive ~consider_order term file =
  let normalize s =
    if case_sensitive then s else String.lowercase_ascii s
  in
  let term = normalize term in
  let term_sorted = List.sort compare (String.to_seq term |> List.of_seq) in

  let contains_term line =
    let line = normalize line in
    if consider_order then
      try
        let line_len = String.length line in
        let term_len = String.length term in
        let rec check_substring i =
          if i + term_len > line_len then false
          else if String.sub line i term_len = term then true
          else check_substring (i + 1)
        in
        check_substring 0
      with _ -> false
    else
      let line_sorted = List.sort compare (String.to_seq line |> List.of_seq) in
      List.exists (fun sublist -> sublist = term_sorted) (subsequences line_sorted)
  in

  let ic = open_in file in
  let rec process_lines line_num acc =
    try
      let line = input_line ic in
      let new_acc = if contains_term line then (line_num, line) :: acc else acc in
      process_lines (line_num + 1) new_acc
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  process_lines 1 []

let print_matches matches =
  List.iter
    (fun (line_num, line) -> Printf.printf "%d: %s\n" line_num line)
    matches;;

