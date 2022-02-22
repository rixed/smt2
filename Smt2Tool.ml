(* Reads an SMT2 on stdin and pretty print it on stdout: *)

let main =
  let debug = Array.mem "--debug" Sys.argv in
  let is_model = Array.mem "--model" Sys.argv in
  let is_sat = Array.mem "--sat" Sys.argv in
  ignore (Parsing.set_trace debug) ;
  let lexbuf = Lexing.from_channel stdin in
  try
    if is_model then
      let resps = Smt2Parser.get_model_response Smt2Lexer.token lexbuf in
      Format.printf "%a@\n" Smt2Types.Response.print resps
    else if is_sat then
      let resp = Smt2Parser.check_sat_response Smt2Lexer.token lexbuf in
      Format.printf "%a@\n" Smt2Types.Response.print resp
    else
      let comms = Smt2Parser.commands Smt2Lexer.token lexbuf in
      Format.printf "%a@\n" Smt2Types.Script.print_commands comms
  with (Failure _ | Parsing.Parse_error) as e ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Format.eprintf "At line %d col %d: %s@."
      pos.Lexing.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
      (Printexc.to_string e)
