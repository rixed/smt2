(* Reads an SMT2 on stdin and pretty print it on stdout: *)

let main =
  let debug = Array.mem "--debug" Sys.argv in
  let is_resp = Array.mem "--resp" Sys.argv in
  ignore (Parsing.set_trace debug) ;
  let lexbuf = Lexing.from_channel stdin in
  if is_resp then
    let resps = Smt2Parser.responses Smt2Lexer.token lexbuf in
    Format.printf "%a@\n" Smt2Types.Script.print_responses resps
  else
    let comms = Smt2Parser.commands Smt2Lexer.token lexbuf in
    Format.printf "%a@\n" Smt2Types.Script.print_commands comms
