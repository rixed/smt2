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
%token RESET_ASSERTIONS SET_INFO SET_LOGIC SET_OPTION NOT

%token KwTheories
%token <string> Numeral Decimal Hexadecimal Binary String
%token <Smt2Types.SpecialConstant.t> SpecConstant
%token <string> Symbol
%token <string> Keyword
%token <Smt2Types.SExpr.t> List

%start commands
%type <Smt2Types.Script.commands> commands

%start responses
%type <Smt2Types.Script.responses> responses
%%

/*
 * S-Expressions
 */

s_expr:
  | spec_constant          { SExpr.SpecConstant $1 }
  | Symbol                 { SExpr.Symbol $1 }
  | Keyword                { SExpr.Keyword $1 }
  | OPEN CLOSE             { SExpr.List [] }
  | OPEN s_expr_list CLOSE { SExpr.List $2 }
;

spec_constant:
  | Numeral     { SpecialConstant.Numeral $1 }
  | Decimal     { SpecialConstant.Decimal $1 }
  | Hexadecimal { SpecialConstant.Hexadecimal $1 }
  | Binary      { SpecialConstant.Binary $1 }
  | String      { SpecialConstant.String $1 }
;

s_expr_list:
  | s_expr s_expr_list { $1 :: $2 }
  | s_expr             { [ $1 ] }
;


/*
 * Identifiers
 */

identifier:
  | Symbol { Identifier.{ symbol = $1 ; indices = [] } }
  | OPEN UNDERSCORE Symbol index_list CLOSE
              { Identifier.{ symbol = $3 ; indices = $4 } }
;

index:
  | Numeral { Identifier.Numeral $1 }
  | Symbol  { Identifier.Symbol $1 }
;

index_list:
  | index index_list { $1 :: $2 }
  | index            { [ $1 ] }
;


/*
 * Attributes
 */

attribute:
  | Keyword                 { Attribute.{ keyword = $1 ; value = None } }
  | Keyword attribute_value { Attribute.{ keyword = $1 ; value = Some $2 } }
;

attribute_list:
  | attribute attribute_list { $1 :: $2 }
  | attribute                { [ $1 ] }
;

attribute_value:
  | spec_constant  { Attribute.SpecConstant $1 }
  | Symbol      { Attribute.Symbol $1 }
  | OPEN CLOSE { Attribute.SExprs [] }
  | OPEN s_expr_list CLOSE { Attribute.SExprs $2 }
;


/*
 * Sorts
 */

sort:
  | identifier { Sort.{ identifier = $1 ; sorts = [] } }
  | OPEN identifier sort_list CLOSE
               { Sort.{ identifier = $2 ; sorts = $3 } }
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
  | identifier { Term.{ identifier = $1 ; sort_opt = None } }
  | OPEN AS identifier sort CLOSE
               { Term.{ identifier = $3 ; sort_opt = Some $4 } }
;

var_binding:
  | OPEN Symbol term CLOSE { Term.{ symbol = $2 ; value = $3 } }
;

sorted_var:
  | OPEN Symbol sort CLOSE { Term.{ name = $2 ; sort = $3 } }
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
  | Symbol                        { [ $1 ] }
  | OPEN Symbol symbol_list CLOSE { $2 :: $3 }
;

symbol_list:
  | Symbol symbol_list { $1 :: $2 }
  | Symbol             { [ $1 ] }
;

symbol_sexpr:
  | OPEN CLOSE             { [] }
  | OPEN symbol_list CLOSE { $2 }
;

match_case:
  | OPEN pattern term CLOSE { Term.{ pattern = $2 ; term = $3 } }
;

match_case_list:
  | match_case match_case_list { $1 :: $2 }
  | match_case                 { [ $1 ] }

term:
  | spec_constant   { Term.SpecConstant $1 }
  | qual_identifier { Term.QualIdentifier $1 }
  | OPEN qual_identifier term_list CLOSE
                    { Term.Apply { identifier = $2 ; terms = $3 } }
  | OPEN LET OPEN var_binding_list CLOSE term CLOSE
                    { Term.Let { bindings = $4 ; term = $6 } }
  | OPEN FORALL OPEN sorted_var_list CLOSE term CLOSE
                    { Term.ForAll { vars = $4 ; term = $6 } }
  | OPEN EXISTS OPEN sorted_var_list CLOSE term CLOSE
                    { Term.Exists { vars = $4 ; term = $6 } }
  | OPEN MATCH term OPEN match_case_list CLOSE CLOSE
                    { Term.Match { term = $3 ; cases = $5 } }
  | OPEN EXCLAMATION term attribute_list CLOSE
                    { Term.Annotation { term = $3 ; attributes = $4 } }
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
  | OPEN Symbol Numeral CLOSE { Command.{ name = $2 ; arity = $3 } }
;

sort_dec_list:
  | sort_dec sort_dec_list { $1 :: $2 }
  | sort_dec               { [ $1 ] }
;

selector_dec:
  | OPEN Symbol sort CLOSE { Command.{ name = $2 ; sort = $3 } }
;

selector_dec_list:
  | selector_dec selector_dec_list { $1 :: $2 }
  | selector_dec                   { [ $1 ] }
;

constructor_dec:
  | OPEN Symbol CLOSE { Command.{ name = $2 ; selectors = [] } }
  | OPEN Symbol selector_dec_list CLOSE
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
  | OPEN Symbol sorted_var_sexpr sort CLOSE
      { Command.{ name = $2 ; inputs = $3 ; output = $4 } }
;

function_dec_list:
  | function_dec function_dec_list { $1 :: $2 }
  | function_dec                   { [ $1 ] }
;

function_def:
  | Symbol sorted_var_sexpr sort term
      { Command.{ dec = { name = $1 ; inputs = $2 ; output = $3 } ;
                  body = $4 } }
;

prop_literal:
  | Symbol                { Command.True $1 }
  | OPEN NOT Symbol CLOSE { Command.False $3 }
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
  | OPEN DECLARE_CONST Symbol sort CLOSE
      { Command.(DeclareConst { name = $3 ; sort = $4 }) }
  | OPEN DECLARE_DATATYPE Symbol datatype_dec CLOSE
      { Command.(DeclareDataType { name = $3 ; datatype = $4 }) }
  | OPEN DECLARE_DATATYPES OPEN sort_dec_list CLOSE
                           OPEN datatype_dec_list CLOSE CLOSE
      { Command.(DeclareDataTypes { sorts = $4 ; datatypes = $7 }) }
  | OPEN DECLARE_FUN Symbol sort_sexpr sort CLOSE
      { Command.(DeclareFun { name = $3 ; inputs = $4 ; output = $5 }) }
  | OPEN DECLARE_SORT Symbol Numeral CLOSE
      { Command.(DeclareSort { name = $3 ; arity = $4 }) }
  | OPEN DEFINE_FUN function_def CLOSE
      { Command.(DefineFun { recurs = false ; def = $3 }) }
  | OPEN DEFINE_FUN_REC function_def CLOSE
      { Command.(DefineFun { recurs = true ; def = $3 }) }
  | OPEN DEFINE_FUNS_REC OPEN function_dec_list CLOSE
                         OPEN term_list CLOSE CLOSE
      { Command.(DefineFuns { decls = $4 ; bodies = $7 }) }
  | OPEN DEFINE_SORT Symbol symbol_sexpr sort CLOSE
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
  | OPEN SET_LOGIC Symbol CLOSE
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
  | OPEN Symbol b_value CLOSE { $2, $3 }
;

t_valuation_pair_list:
  | t_valuation_pair t_valuation_pair_list { $1 :: $2 }
  | t_valuation_pair                       { [ $1 ] }
;

check_sat_response:
  | SAT     { Response.Sat }
  | UNSAT   { Response.Unsat }
  | UNKNOWN { Response.Unknown }
;

echo_response:
  | String  { $1 }
;

get_assertions_response:
  | term_sexpr { $1 }
;

get_assignment_response:
  | OPEN CLOSE                       { [] }
  | OPEN t_valuation_pair_list CLOSE { $2 }
;

get_info_response:
  | OPEN info_response_list CLOSE { $2 }
;

get_model_response:
  | OPEN CLOSE                     { [] }
  | OPEN model_response_list CLOSE { $2 }
;

get_option_response:
  | attribute_value { $1 }
;

get_proof_response:
  | s_expr { $1 }
;

get_unsat_assumptions_response:
  | symbol_sexpr { $1 }
;

get_unsat_core_response:
  | symbol_sexpr { $1 }
;

get_value_response:
  | OPEN valuation_pair_list CLOSE { $2 }
;

specific_success_response:
  | check_sat_response             { Response.CheckSat $1 }
  | echo_response                  { Response.Echo $1 }
  | get_assertions_response        { Response.GetAssertions $1 }
  | get_assignment_response        { Response.GetAssignment $1 }
  | get_info_response              { Response.GetInfo $1 }
  | get_model_response             { Response.GetModel $1 }
  | get_option_response            { Response.GetOption $1 }
  | get_proof_response             { Response.GetProof $1 }
  | get_unsat_assumptions_response { Response.GetUnsatAssumptions $1 }
  | get_unsat_core_response        { Response.GetUnsatCore $1 }
  | get_value_response             { Response.GetValue $1 }
;

general_response:
  | SUCCESS                   { Response.Success }
  | specific_success_response { $1 }
  | UNSUPPORTED               { Response.Unsupported }
  | OPEN ERROR String CLOSE   { Response.Error $3 }
;

responses:
  | EOF                        { [] }
  | general_response responses { $1 :: $2 }
;
