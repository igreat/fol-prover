open Prover.Main
open Prover.Tableau

(** [parse_file filename] reads and parses the file [filename]. *)
let parse_file filename =
  let channel = open_in filename in
  let formula_str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  parse formula_str

(** CLI entry point *)
let () =
  let filename = ref "" in
  let check_satisfiable = ref false in
  let check_valid = ref false in
  let display_tableau = ref false in

  (* TODO: also allow user to choose max constants *)
  let usage_msg = "Usage: prover --satisfiable --valid --tableau <path_to_file>" in

  let speclist = [
    ("--satisfiable", Arg.Set check_satisfiable, "Check if the formula is satisfiable");
    ("--valid", Arg.Set check_valid, "Check if the formula is valid");
    ("--tableau", Arg.Set display_tableau, "Display the tableau");
  ] in

  Arg.parse speclist (fun s -> filename := s) usage_msg;

  if !filename = "" then begin
    prerr_endline "Error: No input file provided.";
    print_endline usage_msg;
    exit 1
  end;

  let formula =
    try parse_file !filename
    with _ ->
      prerr_endline "Error: Unable to parse the formula from the file.";
      exit 1
  in

  if !check_satisfiable then
    print_endline (if is_satisfiable formula then "Satisfiable" else "Unsatisfiable");
  if !check_valid then
    print_endline (if is_valid formula then "Valid" else "Invalid");
  if !display_tableau then
    print_endline (string_of_tableau (expand (Env.empty, 0) formula));

  if not !check_satisfiable && not !check_valid && not !display_tableau then begin
    prerr_endline "Error: No action specified.";
    print_endline usage_msg;
    exit 1
  end
