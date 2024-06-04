
open Farmer_lib

let parse_assocs filepath =
  let contents = In_channel.(with_open_bin filepath input_all) in
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  try
    Parser.assocs Lexer.read lexbuf
  with Parser.Error ->
    Printf.ksprintf failwith
      "Syntax error at %s" (Lexer.show_lexbuf lexbuf)

let () =
  try
    if Array.length Sys.argv < 2 then failwith "Not enough arguments" else
      let assocs = parse_assocs Sys.argv.(1)
      and cwd = Sys.getcwd () in
      List.iter (Association.symlink cwd) assocs
  with
    Failure msg | Sys_error msg ->
      Printf.eprintf "\027[31mFailure\027[0m: %s\n" msg;
      exit 1
