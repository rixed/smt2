%{
open Smt2Types
%}

%token EOF
%token OPEN CLOSE
%token BINARY DECIMAL HEXADECIMAL NUMERAL STRING UNDERSCORE EXCLAMATION AS LET
%token EXISTS FORALL MATCH PAR

%token ERROR FALSE LOGIC
%token SAT SUCCESS THEORY TRUE UNKNOWN UNSUPPORTED UNSAT

%token ASSERT CHECK_SAT CHECK_SAT_ASSUMING
%token DECLARE_CONST DECLARE_DATATYPE DECLARE_DATATYPES DECLARE_FUN DECLARE_SORT
%token DEFINE_FUN DEFINE_FUN_REC DEFINE_FUNS_REC DEFINE_SORT ECHO EXIT
%token GET_ASSERTIONS GET_ASSIGNMENT GET_INFO GET_MODEL GET_OPTION GET_PROOF
%token GET_UNSAT_ASSUMPTIONS GET_UNSAT_CORE GET_VALUE POP PUSH RESET
%token RESET_ASSERTIONS SET_INFO SET_LOGIC SET_OPTION NOT MODEL

%token KwTheories
%token <int> Numeral Hexadecimal Binary
%token <float > Decimal
%token <string> String
%token <Smt2Types.Constant.t> SpecConstant
%token <string> Symbol
%token <string> Keyword
%token <Smt2Types.SExpr.t> List

%start commands
%type <Smt2Types.Script.commands> commands

%start responses
%type <Smt2Types.Script.responses> responses

/* It is actually not possible to parse a response with no knowledge of what
 * the command was (for instance: 'sat' could be CheckSat, if answering
 * `(check-sat)`, or a single symbol after `(get-proof)`. */
%start check_sat_response
%start echo_response
%start get_assertions_response
%start get_assignment_response
%start get_info_response
%start get_model_response
%start get_option_response
%start get_proof_response
%start get_unsat_assumptions_response
%start get_unsat_core_response
%start get_value_response
%start general_response
%type <Smt2Types.Response.t> check_sat_response echo_response get_assertions_response get_assignment_response get_info_response get_model_response get_option_response get_proof_response get_unsat_assumptions_response get_unsat_core_response get_value_response general_response

%%

/*
 * Symbols
 */

symbol:
  | Symbol                { $1 }
  /* Those are not reserved words: */
  | ERROR                 { "error" }
  | FALSE                 { "false" }
  | LOGIC                 { "logic" }
  | SAT                   { "sat" }
  | SUCCESS               { "success" }
  | THEORY                { "theory" }
  | TRUE                  { "true" }
  | UNSUPPORTED           { "unsupported" }
  | UNSAT                 { "unsat" }
  | ASSERT                { "assert" }
  | CHECK_SAT             { "check-sat" }
  | CHECK_SAT_ASSUMING    { "check-sat-assuming" }
  | DECLARE_CONST         { "declare-const" }
  | DECLARE_DATATYPE      { "declare-datatype" }
  | DECLARE_DATATYPES     { "declare-datatypes" }
  | DECLARE_FUN           { "declare-fun" }
  | DECLARE_SORT          { "declare-sort" }
  | DEFINE_FUN            { "define-fun" }
  | DEFINE_FUN_REC        { "define-fun-rec" }
  | DEFINE_FUNS_REC       { "define-funs-rec" }
  | DEFINE_SORT           { "define-sort" }
  | ECHO                  { "echo" }
  | EXIT                  { "exit" }
  | GET_ASSERTIONS        { "get-assertions" }
  | GET_ASSIGNMENT        { "get-assignment" }
  | GET_INFO              { "get-info" }
  | GET_MODEL             { "get-model" }
  | GET_OPTION            { "get-option" }
  | GET_PROOF             { "get-proof" }
  | GET_UNSAT_ASSUMPTIONS { "get-unsat-assumptions" }
  | GET_UNSAT_CORE        { "get-unsat-core" }
  | GET_VALUE             { "get-value" }
  | POP                   { "pop" }
  | PUSH                  { "push" }
  | RESET                 { "reset" }
  | RESET_ASSERTIONS      { "reset-assertions" }
  | SET_INFO              { "set-info" }
  | SET_LOGIC             { "set-logic" }
  | SET_OPTION            { "set-option" }
  | NOT                   { "not" }
  | MODEL                 { "model" }
;

/*
 * S-Expressions
 */

s_expr:
  | spec_constant          { Constant $1 }
  | symbol                 { Symbol $1 }
  | Keyword                { Keyword $1 }
  | OPEN CLOSE             { List [] }
  | OPEN s_expr_list CLOSE { List $2 }
;

spec_constant:
  | Numeral     { Numeral $1 }
  | Decimal     { Decimal $1 }
  | Hexadecimal { Hexadecimal $1 }
  | Binary      { Binary $1 }
  | String      { String $1 }
;

s_expr_list:
  | s_expr s_expr_list { $1 :: $2 }
  | s_expr             { [ $1 ] }
;


/*
 * Identifiers
 */

identifier:
  | symbol { Identifier $1}
  | OPEN UNDERSCORE symbol index_list CLOSE
           { IndexedIdentifier ($3, $4) }
;

index:
  | Numeral { NumericIndex $1 }
  | symbol  { SymbolicIndex $1 }
;

index_list:
  | index index_list { $1 :: $2 }
  | index            { [ $1 ] }
;


/*
 * Attributes
 */

attribute:
  | Keyword                 { $1, None }
  | Keyword attribute_value { $1, Some $2 }
;

attribute_list:
  | attribute attribute_list { $1 :: $2 }
  | attribute                { [ $1 ] }
;

attribute_value:
  | spec_constant          { ConstantValue $1 }
  | symbol                 { SymbolicValue $1 }
  | OPEN CLOSE             { SExprValue [] }
  | OPEN s_expr_list CLOSE { SExprValue $2 }
;


/*
 * Sorts
 */

sort:
  | identifier                      { NonParametricSort $1 }
  | OPEN identifier sort_list CLOSE { ParametricSort ($2, $3) }
;

sort_list:
  | sort sort_list { $1 :: $2 }
  | sort           { [ $1 ] }
;

sort_sexpr:
  | OPEN CLOSE           { [] }
  | OPEN sort_list CLOSE { $2 }
;


/*
 * Terms and Formulas
 */

qual_identifier:
  | identifier                    { $1, None }
  | OPEN AS identifier sort CLOSE { $3, Some $4 }
;

var_binding:
  | OPEN symbol term CLOSE { $2, $3 }
;

sorted_var:
  | OPEN symbol sort CLOSE { $2, $3 }
;

sorted_var_list:
  | sorted_var sorted_var_list { $1 :: $2 }
  | sorted_var                 { [ $1 ] }
;

sorted_var_sexpr:
  | OPEN CLOSE                 { [] }
  | OPEN sorted_var_list CLOSE { $2 }
;

pattern:
  | symbol                        { [ $1 ] }
  | OPEN symbol symbol_list CLOSE { $2 :: $3 }
;

symbol_list:
  | symbol symbol_list { $1 :: $2 }
  | symbol             { [ $1 ] }
;

symbol_sexpr:
  | OPEN CLOSE             { [] }
  | OPEN symbol_list CLOSE { $2 }
;

match_case:
  | OPEN pattern term CLOSE { $2, $3 }
;

match_case_list:
  | match_case match_case_list { $1 :: $2 }
  | match_case                 { [ $1 ] }

term:
  | spec_constant                                     { ConstantTerm $1 }
  | qual_identifier                                   { QualIdentifier $1 }
  | OPEN qual_identifier term_list CLOSE              { Apply ($2, $3) }
  | OPEN LET OPEN var_binding_list CLOSE term CLOSE   { Let ($4, $6) }
  | OPEN FORALL OPEN sorted_var_list CLOSE term CLOSE { ForAll ($4, $6) }
  | OPEN EXISTS OPEN sorted_var_list CLOSE term CLOSE { Exists ($4, $6) }
  | OPEN MATCH term OPEN match_case_list CLOSE CLOSE  { Match ($3, $5) }
  | OPEN EXCLAMATION term attribute_list CLOSE        { Annotation ($3, $4) }
;

term_list:
  | term term_list { $1 :: $2 }
  | term           { [ $1 ] }
;

term_sexpr:
  | OPEN CLOSE           { [] }
  | OPEN term_list CLOSE { $2 }
;

var_binding_list:
  | var_binding var_binding_list { $1 :: $2 }
  | var_binding                  { [ $1 ] }
;


/*
 * Scripts
 */

sort_dec:
  | OPEN symbol Numeral CLOSE { Command.{ name = $2 ; arity = $3 } }
;

sort_dec_list:
  | sort_dec sort_dec_list { $1 :: $2 }
  | sort_dec               { [ $1 ] }
;

selector_dec:
  | OPEN symbol sort CLOSE { Command.{ name = $2 ; sort = $3 } }
;

selector_dec_list:
  | selector_dec selector_dec_list { $1 :: $2 }
  | selector_dec                   { [ $1 ] }
;

constructor_dec:
  | OPEN symbol CLOSE { Command.{ name = $2 ; selectors = [] } }
  | OPEN symbol selector_dec_list CLOSE
                      { Command.{ name = $2 ; selectors = $3 } }
;

constructor_dec_list:
  | constructor_dec constructor_dec_list { $1 :: $2 }
  | constructor_dec                      { [ $1 ] }
;

datatype_dec:
  | OPEN constructor_dec_list CLOSE
      { Command.NonParametric $2 }
  | OPEN PAR OPEN symbol_list CLOSE OPEN constructor_dec_list CLOSE CLOSE
      { Command.Parametric { parameters = $4 ; constructors = $7 } }
;

datatype_dec_list:
  | datatype_dec datatype_dec_list { $1 :: $2 }
  | datatype_dec                   { [ $1 ] }
;

function_dec:
  | OPEN symbol sorted_var_sexpr sort CLOSE
      { Command.{ name = $2 ; inputs = $3 ; output = $4 } }
;

function_dec_list:
  | function_dec function_dec_list { $1 :: $2 }
  | function_dec                   { [ $1 ] }
;

function_def:
  | symbol sorted_var_sexpr sort term
      { Command.{ dec = { name = $1 ; inputs = $2 ; output = $3 } ;
                  body = $4 } }
;

prop_literal:
  | symbol                { Command.True $1 }
  | OPEN NOT symbol CLOSE { Command.False $3 }
;

prop_literal_list:
  | prop_literal prop_literal_list { $1 :: $2 }
  | prop_literal                   { [ $1 ] }
;

prop_literal_sexpr:
  | OPEN CLOSE                   { [] }
  | OPEN prop_literal_list CLOSE { $2 }
;

b_value:
  | TRUE  { true }
  | FALSE { false }
;

option:
  | Keyword String  { Command.StringOption ($1, $2) }
  | Keyword b_value { Command.BoolOption ($1, $2) }
  | Keyword Numeral { Command.NumOption ($1, $2) }
  | attribute       { Command.AttributeOption $1 }
;

command:
  | OPEN ASSERT term CLOSE
      { Command.Assert $3 }
  | OPEN CHECK_SAT CLOSE
      { Command.CheckSat }
  | OPEN CHECK_SAT_ASSUMING prop_literal_sexpr CLOSE
      { Command.CheckSatAssuming $3 }
  | OPEN DECLARE_CONST symbol sort CLOSE
      { Command.(DeclareConst { name = $3 ; sort = $4 }) }
  | OPEN DECLARE_DATATYPE symbol datatype_dec CLOSE
      { Command.(DeclareDataType { name = $3 ; datatype = $4 }) }
  | OPEN DECLARE_DATATYPES OPEN sort_dec_list CLOSE
                           OPEN datatype_dec_list CLOSE CLOSE
      { Command.(DeclareDataTypes { sorts = $4 ; datatypes = $7 }) }
  | OPEN DECLARE_FUN symbol sort_sexpr sort CLOSE
      { Command.(DeclareFun { name = $3 ; inputs = $4 ; output = $5 }) }
  | OPEN DECLARE_SORT symbol Numeral CLOSE
      { Command.(DeclareSort { name = $3 ; arity = $4 }) }
  | OPEN DEFINE_FUN function_def CLOSE
      { Command.(DefineFun { recurs = false ; def = $3 }) }
  | OPEN DEFINE_FUN_REC function_def CLOSE
      { Command.(DefineFun { recurs = true ; def = $3 }) }
  | OPEN DEFINE_FUNS_REC OPEN function_dec_list CLOSE
                         OPEN term_list CLOSE CLOSE
      { Command.(DefineFuns { decls = $4 ; bodies = $7 }) }
  | OPEN DEFINE_SORT symbol symbol_sexpr sort CLOSE
      { Command.(DefineSort { name = $3 ; params = $4 ; sort = $5 }) }
  | OPEN ECHO String CLOSE
      { Command.Echo $3 }
  | OPEN EXIT CLOSE
      { Command.Exit }
  | OPEN GET_ASSERTIONS CLOSE
      { Command.GetAssertions }
  | OPEN GET_ASSIGNMENT CLOSE
      { Command.GetAssignment }
  | OPEN GET_INFO Keyword CLOSE
      { Command.GetInfo $3 }
  | OPEN GET_MODEL CLOSE
      { Command.GetModel }
  | OPEN GET_PROOF CLOSE
      { Command.GetProof }
  | OPEN GET_UNSAT_ASSUMPTIONS CLOSE
      { Command.GetUnsatAssumptions }
  | OPEN GET_UNSAT_CORE CLOSE
      { Command.GetUnsatCore }
  | OPEN GET_VALUE OPEN term_list CLOSE CLOSE
      { Command.GetValue $4 }
  | OPEN POP Numeral CLOSE
      { Command.Pop $3 }
  | OPEN PUSH Numeral CLOSE
      { Command.Push $3 }
  | OPEN RESET CLOSE
      { Command.Reset }
  | OPEN RESET_ASSERTIONS CLOSE
      { Command.ResetAssertions }
  | OPEN SET_INFO attribute CLOSE
      { Command.SetInfo $3 }
  | OPEN SET_LOGIC symbol CLOSE
      { Command.SetLogic $3 }
  | OPEN SET_OPTION option CLOSE
      { Command.SetOption $3 }
;

commands:
  | EOF              { [] }
  | command commands { $1 :: $2 }
;

/*
 * Command Responses
 */

model_response:
  | OPEN DEFINE_FUN function_def CLOSE
      { Response.(DefineFun { recurs = false ; def = $3 }) }
  | OPEN DEFINE_FUN_REC function_def CLOSE
      { Response.(DefineFun { recurs = true ; def = $3 }) }
  | OPEN DEFINE_FUNS_REC OPEN function_dec_list CLOSE OPEN term_list CLOSE CLOSE
      { Response.(DefineFuns { decls = $4 ; bodies = $7 }) }
;

model_response_list:
  | model_response model_response_list { $1 :: $2 }
  | model_response                     { [ $1 ] }
;

info_response:
  | Keyword Numeral { Response.NumResp ($1, $2) }
  | Keyword String  { Response.StringResp ($1, $2) }
  | attribute       { Response.AttributeResp $1 }
;

info_response_list:
  | info_response info_response_list { $1 :: $2 }
  | info_response                    { [ $1 ] }
;

valuation_pair:
  | OPEN term term CLOSE { $2, $3 }
;

valuation_pair_list:
  | valuation_pair valuation_pair_list { $1 :: $2 }
  | valuation_pair                     { [ $1 ] }
;

t_valuation_pair:
  | OPEN symbol b_value CLOSE { $2, $3 }
;

t_valuation_pair_list:
  | t_valuation_pair t_valuation_pair_list { $1 :: $2 }
  | t_valuation_pair                       { [ $1 ] }
;

check_sat_response:
  | SAT              { Response.CheckSat Sat }
  | UNSAT            { Response.CheckSat Unsat }
  | UNKNOWN          { Response.CheckSat Unknown }
  | general_response { $1 }
;

echo_response:
  | String           { Response.Echo $1 }
  | general_response { $1 }
;

get_assertions_response:
  | term_sexpr       { Response.GetAssertions $1 }
  | general_response { $1 }
;

get_assignment_response:
  | OPEN CLOSE                       { Response.GetAssignment [] }
  | OPEN t_valuation_pair_list CLOSE { Response.GetAssignment $2 }
  | general_response                 { $1 }
;

get_info_response:
  | OPEN info_response_list CLOSE { Response.GetInfo $2 }
  | general_response              { $1 }
;

get_model_response:
  | OPEN CLOSE                           { Response.GetModel [] }
  | OPEN MODEL CLOSE                     { Response.GetModel [] }
  | OPEN model_response_list CLOSE       { Response.GetModel $2 }
  | OPEN MODEL model_response_list CLOSE { Response.GetModel $3 }
  | general_response                     { $1 }
;

get_option_response:
  | attribute_value  { Response.GetOption $1 }
  | general_response { $1 }
;

get_proof_response:
  | s_expr           { Response.GetProof $1 }
  | general_response { $1 }
;

get_unsat_assumptions_response:
  | symbol_sexpr     { Response.GetUnsatAssumptions $1 }
  | general_response { $1 }
;

get_unsat_core_response:
  | symbol_sexpr     { Response.GetUnsatCore $1 }
  | general_response { $1 }
;

get_value_response:
  | OPEN valuation_pair_list CLOSE { Response.GetValue $2 }
  | general_response               { $1 }
;

general_response:
  | SUCCESS                   { Response.Success }
  | UNSUPPORTED               { Response.Unsupported }
  | OPEN ERROR String CLOSE   { Response.Error $3 }
;

responses:
  | EOF                        { [] }
  | general_response responses { $1 :: $2 }
;
