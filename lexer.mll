{
  open! Lexing
  open Parser

  exception Unknown_character of char

  let show_lexbuf lexbuf =
    let p = lexbuf.lex_start_p in
    Printf.sprintf "file %s, line %d, column %d"
      (p.pos_fname) (p.pos_lnum) (p.pos_cnum - p.pos_bol + 1)
}

let whitespace = ' ' | '\t' | '\r'
let newline = '\n'

let filepath = ['a'-'z' 'A'-'Z' '0'-'9' '$' '.' '-' '_' '/']+

rule read =
  parse whitespace    { read lexbuf }
      | '#'           { comment lexbuf }
      | ':'           { COLON }
      | newline       { new_line lexbuf; read lexbuf }
      | eof           { EOF }
      | filepath as f { let f' =
                          let env_var = Str.regexp {|\$\([A-Za-z_]+\)|}
                          and replace_env_var = fun var ->
                            let name = Str.matched_group 1 var in 
                            try Sys.getenv name
                            with Not_found ->
                              Printf.ksprintf failwith
                                "Environment variable $%s is not set. Happened at %s"
                                name (show_lexbuf lexbuf)
                          in
                          Str.global_substitute env_var replace_env_var f
                        in
                        FILEPATH f' }
      | _ as c        { raise (Unknown_character c) }

and comment =
  parse newline | eof { new_line lexbuf; read lexbuf }
      | _             { comment lexbuf }
