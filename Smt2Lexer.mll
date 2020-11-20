{
open Smt2Parser
}

let comment = ';' (_ # '\n') * '\n'
let white_space_char = [ '\t' '\n' '\r' ' ' ]
let printable_char = [ ' ' - '~' ]
let digit = [ '0' - '9' ]
let letter = [ 'A' - 'Z' ] | [ 'a' - 'z' ]
let numeral = '0' | [ '1' - '9' ] digit *
let decimal = numeral '.' '0'* numeral
let hexadecimal = "#x" ((digit | [ 'a' - 'f' ] | [ 'A' - 'F' ]) + as s)
let binary = "#b" ([ '0' '1' ] + as s)
let string = '"' (((printable_char # '"') | white_space_char | '"' '"')* as s) '"'
let symchar =
  [ '~' '!' '@' '$' '%' '^' '&' '*' '_' '-' '+' '=' '<' '>' '.' '?' '/' ]
let simple_symbol =
  (letter | symchar) (letter | digit | symchar) + as s
let quoted_symbol =
  '|' ((white_space_char | (printable_char # [ '|' '\\' ])) * as s) '|'
let symbol = simple_symbol | quoted_symbol
let keyword = ':' (simple_symbol as s)

rule token = parse
  | white_space_char { token lexbuf }
  | comment { token lexbuf }
  | numeral as s { Numeral s }
  | decimal as s { Decimal s }
  | hexadecimal { Hexadecimal s }
  | binary { Binary s }
  | string { String s }
  (* Test for reserved words *before* symbols *)
  | "BINARY" { BINARY }
  | "DECIMAL" { DECIMAL }
  | "HEXADECIMAL" { HEXADECIMAL }
  | "NUMERAL" { NUMERAL }
  | "STRING" { STRING }
  | '_' { UNDERSCORE }
  | '!' { EXCLAMATION }
  | "as" { AS }
  | "let" { LET }
  | "exists" { EXISTS }
  | "forall" { FORALL }
  | "match" { MATCH }
  | "par" { PAR }
  | symbol { Symbol s }
  | ":theories" { KwTheories }
  | keyword { Keyword s }
  | '(' { OPEN }
  | ')' { CLOSE }
  | eof { EOF }
  (* predefined symbols *)
  | "error" { ERROR }
  | "false" { FALSE }
  | "logic" { LOGIC }
  | "sat" { SAT }
  | "success" { SUCCESS }
  | "theory" { THEORY }
  | "true" { TRUE }
  | "unsupported" { UNSUPPORTED }
  | "unsat" { UNSAT }
  (* Command names *)
  | "assert" { ASSERT }
  | "check-sat" { CHECK_SAT }
  | "check-sat-assuming" { CHECK_SAT_ASSUMING }
  | "declare-const" { DECLARE_CONST }
  | "declare-datatype" { DECLARE_DATATYPE }
  | "declare-datatypes" { DECLARE_DATATYPES }
  | "declare-fun" { DECLARE_FUN }
  | "declare-sort" { DECLARE_SORT }
  | "define-fun" { DEFINE_FUN }
  | "define-fun-rec" { DEFINE_FUN_REC }
  | "define-funs-rec" { DEFINE_FUNS_REC }
  | "define-sort" { DEFINE_SORT }
  | "echo" { ECHO }
  | "exit" { EXIT }
  | "get-assertions" { GET_ASSERTIONS }
  | "get-assignment" { GET_ASSIGNMENT }
  | "get-info" { GET_INFO }
  | "get-model" { GET_MODEL }
  | "get-option" { GET_OPTION }
  | "get-proof" { GET_PROOF }
  | "get-unsat-assumptions" { GET_UNSAT_ASSUMPTIONS }
  | "get-unsat-core" { GET_UNSAT_CORE }
  | "get-value" { GET_VALUE }
  | "pop" { POP }
  | "push" { PUSH }
  | "reset" { RESET }
  | "reset-assertions" { RESET_ASSERTIONS }
  | "set-info" { SET_INFO }
  | "set-logic" { SET_LOGIC }
  | "set-option" { SET_OPTION }
  (* Felt through the cracks: *)
  | "not" { NOT }
