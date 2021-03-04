open Big_int

type symbol = string
type keyword = string
type numeral = big_int
type decimal = float

let symbol_of_string s = s
let numeral_of_int n = big_int_of_int n
let keyword_of_string s = s

(* Replace newlines with something inocuous: *)
let strip_newlines s =
  String.split_on_char '\n' s |>
  String.concat " "

(* print a list of things, separated with spaces: *)
let print_list_content p oc l =
  let first = ref true in
  List.iter (fun s ->
    Format.fprintf oc
      (if !first then (first := false ; "%a") else "@ %a")
      p s ;
  ) l

let print_list p oc l =
  Format.fprintf oc "@[<hov 2>(" ;
  print_list_content p oc l ;
  Format.fprintf oc ")@]"

let print_symbol oc s =
  (* TODO: maybe quote with '|': *)
  Format.fprintf oc "%s" s

let print_keyword oc s =
  Format.fprintf oc ":%s" s

let print_string oc s =
  Format.fprintf oc "\"%s\"" s (* '"'s are still doubled *)

let print_bool oc b =
  Format.fprintf oc (if b then "true" else "false")

let print_numeral oc n =
  string_of_big_int n |>
  Format.fprintf oc "%s"

let print_decimal oc f =
  Format.fprintf oc "%f" f

let rec hexdigits_of_num n =
  let mask = big_int_of_int 15 in
  let rec loop l n =
    if eq_big_int n zero_big_int then List.rev l else
    let l' = int_of_big_int (and_big_int n mask) :: l in
    loop l' (shift_right_towards_zero_big_int n 4) in
  loop [] n

let print_hexadecimal oc n =
  let digits = hexdigits_of_num n in
  let digits = if digits = [] then [ 0 ] else digits in
  Format.pp_print_string oc "#x" ;
  List.iter (fun d ->
   Format.pp_print_char oc (Char.chr (Char.code '0' + d))
  ) digits

let rec bits_of_num n =
  let rec loop l n =
    if eq_big_int n zero_big_int then List.rev l else
    let l' = (if eq_big_int (and_big_int n unit_big_int) zero_big_int then 0 else 1) :: l in
    loop l' (shift_right_towards_zero_big_int n 1) in
  loop [] n

let print_binary oc n =
  let bits = bits_of_num n in
  let bits = if bits = [] then [ 0 ] else bits in
  Format.pp_print_string oc "#b" ;
  List.iter (fun b ->
    Format.pp_print_char oc (if b = 0 then '0' else '1')
  ) bits

(* When the first of the pair is a keyword: *)
let print_pair p2 oc k v =
  Format.fprintf oc "%a@ %a"
    print_keyword k
    p2 v

type constant =
  Numeral of numeral | Decimal of decimal | Hexadecimal of big_int |
  Binary of big_int | String of string

module Constant = struct
  type t = constant

  let numeral_of_int v = Numeral (numeral_of_int v)
  let decimal_of_int v = Decimal (float_of_int v)
  let decimal_of_float v = Decimal v

  let to_big_int = function
    | Numeral n | Hexadecimal n | Binary n -> n
    | _ -> invalid_arg "Constant.to_int"

  let print oc = function
      Numeral v -> print_numeral oc v
    | Decimal f -> print_decimal oc f
    | Hexadecimal n -> print_hexadecimal oc n
    | Binary n -> print_binary oc n
    | String s -> print_string oc s
end

type s_expr =
    Constant of constant
  | Symbol of symbol
  | Keyword of keyword
  | List of s_expr list
  | Comment of string (* Not parsed but pretty printed *)

module SExpr = struct
  type t = s_expr

  let comment s = Comment s

  let rec print oc = function
      Constant s -> Constant.print oc s
    | Symbol s -> Format.fprintf oc "%a" print_symbol s
    | Keyword k -> Format.fprintf oc "%a" print_keyword k
    | List ss -> print_list print oc ss
    | Comment s -> Format.fprintf oc "@[<h>; %s@]@\n" (strip_newlines s)
end

type index = NumericIndex of int | SymbolicIndex of symbol
type identifier =
    Identifier of symbol
  | IndexedIdentifier of symbol * index list (* non empty *)

module Identifier = struct
  type t = identifier

  let of_string s = Identifier s

  let print_index oc = function
      NumericIndex i -> Format.fprintf oc "%d" i
    | SymbolicIndex s  -> print_symbol oc s

  let print oc = function
      Identifier name ->
        Format.fprintf oc "%s" name
    | IndexedIdentifier (name, indices) ->
        Format.fprintf oc "@[<hov 2>(_@ %s@ %a)@]"
          name
          (print_list_content print_index) indices
end

type attribute_value =
    ConstantValue of constant
  | SymbolicValue of symbol
  | SExprValue of s_expr list

type attribute = keyword * attribute_value option

module Attribute = struct
  type t = attribute

  let print_value oc = function
    | ConstantValue s -> Constant.print oc s
    | SymbolicValue s -> print_symbol oc s
    | SExprValue l -> print_list SExpr.print oc l

  let print oc = function
      keyword, None ->
        Format.fprintf oc ":%s" keyword
    | keyword, Some v ->
        Format.fprintf oc ":%s@ %a" keyword print_value v
end

type sort =
    NonParametricSort of identifier
  | ParametricSort of identifier * sort list (* parameters *)

module Sort = struct
  type t = sort

  let of_string s =
    NonParametricSort (Identifier.of_string s)

  let rec print oc = function
      NonParametricSort id ->
        Identifier.print oc id
    | ParametricSort (id, params) ->
        Format.fprintf oc "@[(%a@ %a)@["
          Identifier.print id
          (print_list_content print) params
end

type qual_identifier = identifier * (* as... *) Sort.t option
type sorted_var = symbol * sort
type pattern = symbol list
type match_case = pattern * term
and binding = symbol * term
and term =
    ConstantTerm of constant
  | QualIdentifier of qual_identifier
  | Apply of qual_identifier * term list
  | Let of binding list * term
  | ForAll of sorted_var list * term
  | Exists of sorted_var list * term
  | Match of term * match_case list
  | Annotation of term * attribute list

module Term = struct
  type t = term

  let qual_identifier_of_string s = s, None

  let print_qual_identifier oc = function
      id, None ->
        Identifier.print oc id
    | id, Some sort ->
        Format.fprintf oc "@[(as@ %a@ %a)@]"
          Identifier.print id
          Sort.print sort

  let print_sorted_var oc (name, sort) =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_symbol name
      Sort.print sort

  let rec print_binding oc (name, value) =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_symbol name
      print value

  and print_pattern oc = function
      [ x ] -> print_symbol oc x
    | l -> print_list print_symbol oc l

  and print_case oc (pattern, term) =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_pattern pattern
      print term

  and print oc = function
      ConstantTerm s -> Constant.print oc s
    | QualIdentifier i -> print_qual_identifier oc i
    | Apply (id, terms) ->
        Format.fprintf oc "@[(%a@ %a)@]"
          print_qual_identifier id
          (print_list print) terms
    | Let (bindings, term) ->
        Format.fprintf oc "@[(let@ %a@ %a)@]"
          (print_list print_binding) bindings
          print term
    | ForAll (vars, term) ->
        Format.fprintf oc "@[(forall@ %a@ %a)@]"
          (print_list print_sorted_var) vars
          print term
    | Exists (vars, term) ->
        Format.fprintf oc "@[(exists@ %a@ %a)@]"
          (print_list print_sorted_var) vars
          print term
    | Match (term, cases) ->
        Format.fprintf oc "@[(match@ %a@ %a)@]"
          print term
          (print_list print_case) cases
    | Annotation (term, attributes) ->
        Format.fprintf oc "@[(!@ %a@ %a)@]"
          print term
          (print_list Attribute.print) attributes

  let to_bool = function
    | QualIdentifier (Identifier "true", None) ->
        true
    | QualIdentifier (Identifier "false", None) ->
        false
    | x ->
        Format.(fprintf str_formatter "Bad term when expecting boolean: %a"
          print x) ;
        Format.flush_str_formatter () |> failwith

  let rec to_big_int = function
    | ConstantTerm c ->
        Constant.to_big_int c
    | Apply ((Identifier "-", None), [ term ]) ->
        minus_big_int (to_big_int term)
    | x ->
        Format.(fprintf str_formatter "Bad term when expecting integer: %a"
          print x) ;
        Format.flush_str_formatter () |> failwith
end

module Command = struct
  type option =
    | StringOption of keyword * string
    | BoolOption of keyword * bool
    | NumOption of keyword * numeral
    | AttributeOption of attribute

  type sort_dec = { name : symbol ; arity : numeral }

  type selector_dec = { name : symbol ; sort : Sort.t }

  type constructor_dec = { name : symbol ; selectors : selector_dec list }

  type datatype_dec =
      NonParametric of constructor_dec list
    | Parametric of
        { parameters : symbol list ; constructors : constructor_dec list }

  type function_dec =
    { name : symbol ; inputs : sorted_var list ; output : Sort.t }

  type function_def = { dec : function_dec ; body : Term.t }

  type prop_literal =
    | True of symbol
    | False of symbol

  type t =
      Assert of Term.t
    | CheckSat
    | CheckSatAssuming of prop_literal list
    | DeclareConst of { name : symbol ; sort : Sort.t }
    | DeclareDataType of { name : symbol ; datatype : datatype_dec }
    | DeclareDataTypes of
        { sorts : sort_dec list ; datatypes : datatype_dec list }
    | DeclareFun of { name : symbol ; inputs : Sort.t list ; output : Sort.t }
    | DeclareSort of { name : symbol ; arity : numeral }
    | DefineFun of { recurs : bool ; def : function_def }
    | DefineFuns of
        { decls : function_dec list ; bodies : Term.t list }
    | DefineSort of
        { name : symbol ; params : symbol list ; sort : Sort.t }
    | Echo of string
    | Exit
    | GetAssertions
    | GetAssignment
    | GetInfo of keyword
    | GetModel
    | GetOption of keyword
    | GetProof
    | GetUnsatAssumptions
    | GetUnsatCore
    | GetValue of Term.t list
    | Pop of numeral
    | Push of numeral
    | Reset
    | ResetAssertions
    | SetInfo of attribute
    | SetLogic of symbol
    | SetOption of option

  let set_string_option k v =
    SetOption (StringOption (k, v))
  let set_bool_option k v =
    SetOption (BoolOption (k, v))
  let set_int_option k v =
    SetOption (NumOption (k, numeral_of_int v))
  let print_success v =
    set_bool_option "print-success" v

  let set_logic s =
    SetLogic s

  let print_option oc = function
    | StringOption (k, s) -> print_pair print_string oc k s
    | BoolOption (k, b) -> print_pair print_bool oc k b
    | NumOption (k, n) -> print_pair print_numeral oc k n
    | AttributeOption a -> Attribute.print oc a

  let print_sort_dec oc (s : sort_dec) =
    print_pair print_numeral oc s.name s.arity

  let print_selector_dec oc (s : selector_dec) =
    print_pair Sort.print oc s.name s.sort

  let print_constructor_dec oc (c : constructor_dec) =
    print_pair (print_list print_selector_dec) oc c.name c.selectors

  let print_datatype_dec oc = function
      NonParametric l ->
        print_list print_constructor_dec oc l
    | Parametric { parameters ; constructors } ->
        Format.fprintf oc "@[(par@ %a@ %a)@]"
          (print_list print_symbol) parameters
          (print_list print_constructor_dec) constructors

  let print_function_dec oc f =
    Format.fprintf oc "@[(%a@ %a@ %a)@]"
      print_symbol f.name
      (print_list Term.print_sorted_var) f.inputs
      Sort.print f.output

  let print_function_def oc f =
    Format.fprintf oc "%a@ %a@ %a@ %a"
      print_symbol f.dec.name
      (print_list Term.print_sorted_var) f.dec.inputs
      Sort.print f.dec.output
      Term.print f.body

  let print_prop_literal oc = function
      True s -> print_symbol oc s
    | False s -> Format.fprintf oc "@[(not@ %a)@]" print_symbol s

  let print oc =
    let p0 s =
      Format.fprintf oc "(%s)" s
    and p1 s p1 v1 =
      Format.fprintf oc "@[(%s@ %a)@]" s p1 v1
    and p2 s p1 v1 p2 v2 =
      Format.fprintf oc "@[(%s@ %a@ %a)@]" s p1 v1 p2 v2
    and p3 s p1 v1 p2 v2 p3 v3 =
      Format.fprintf oc "@[(%s@ %a@ %a@ %a)@]" s p1 v1 p2 v2 p3 v3 in
    function
      Assert t ->
        p1 "assert" Term.print t
    | CheckSat ->
        p0 "check-sat"
    | CheckSatAssuming l ->
        p1 "check-sat-assuming"
          (print_list print_prop_literal) l
    | DeclareConst c ->
        p2 "declare-const"
          print_symbol c.name
          Sort.print c.sort
    | DeclareDataType d ->
        p2 "declare-datatype"
          print_symbol d.name
          print_datatype_dec d.datatype
    | DeclareDataTypes d ->
        p2 "declare-datatypes"
          (print_list print_sort_dec) d.sorts
          (print_list print_datatype_dec) d.datatypes
    | DeclareFun f ->
        p3 "declare-fun"
          print_symbol f.name
          (print_list Sort.print) f.inputs
          Sort.print f.output
    | DeclareSort s ->
        p2 "declare-sort"
          print_symbol s.name
          print_numeral s.arity
    | DefineFun f ->
        p1 ("define-fun" ^ if f.recurs then "-rec" else "")
          print_function_def f.def
    | DefineFuns f ->
        p2 "define-funs-rec"
          (print_list print_function_dec) f.decls
          (print_list Term.print) f.bodies
    | DefineSort s ->
        p3 "define-sort"
          print_symbol s.name
          (print_list print_symbol) s.params
          Sort.print s.sort
    | Echo s ->
        p1 "echo" print_string s
    | Exit ->
        p0 "exit"
    | GetAssertions ->
        p0 "get-assertions"
    | GetAssignment ->
        p0 "get-assignment"
    | GetInfo k ->
        p1 "get-info" print_symbol k
    | GetModel ->
        p0 "get-model"
    | GetOption k ->
        p1 "get-option" print_symbol k
    | GetProof ->
        p0 "get-proof"
    | GetUnsatAssumptions ->
        p0 "get-unsat-assumptions"
    | GetUnsatCore ->
        p0 "get-unsat-core"
    | GetValue l ->
        p1 "get-value" (print_list Term.print) l
    | Pop n ->
        p1 "pop" print_numeral n
    | Push n ->
        p1 "push" print_numeral n
    | Reset ->
        p0 "reset"
    | ResetAssertions ->
        p0 "reset-assertions"
    | SetInfo a ->
        p1 "set-info" Attribute.print a
    | SetLogic s ->
        p1 "set-logic" print_symbol s
    | SetOption o ->
        p1 "set-option" print_option o
end

module Response = struct
  type model =
      DefineFun of { recurs : bool ; def : Command.function_def }
    | DefineFuns of
        { decls : Command.function_dec list ; bodies : Term.t list }

  type info =
      StringResp of keyword * string
    | NumResp of keyword * numeral
    | AttributeResp of attribute

  type sat = Sat | Unsat | Unknown

  type t =
      Success
    | CheckSat of sat
    | Echo of string
    | GetAssertions of Term.t list
    | GetAssignment of (symbol * bool) list
    | GetInfo of info list
    | GetModel of model list
    | GetOption of attribute_value
    | GetProof of SExpr.t
    | GetUnsatAssumptions of symbol list
    | GetUnsatCore of symbol list
    | GetValue of (Term.t * Term.t) list
    | Unsupported
    | Error of string

  let print_model oc = function
      DefineFun f ->
        Format.fprintf oc "@[(define-fun%s@ %a)@]"
          (if f.recurs then "-rec" else "")
          Command.print_function_def f.def
    | DefineFuns f ->
        Format.fprintf oc "@[(define-funs-rec@ %a@ %a)@]"
          (print_list Command.print_function_dec) f.decls
          (print_list Term.print) f.bodies

  let print_info oc = function
      StringResp (k, s) -> print_pair print_string oc k s
    | NumResp (k, n) -> print_pair print_numeral oc k n
    | AttributeResp a -> Attribute.print oc a

  let print_sat oc s =
    Format.pp_print_string oc (match s with
        Sat -> "sat"
      | Unsat -> "unsat"
      | Unknown -> "unknown")

  let print_t_valuation_pair oc (s, b) =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_symbol s
      print_bool b

  let print_valuation_pair oc (t1, t2) =
    Format.fprintf oc "@[(%a@ %a)@]"
      Term.print t1
      Term.print t2

  let print oc = function
      Success -> Format.pp_print_string oc "success"
    | CheckSat s -> print_sat oc s
    | Echo s -> print_string oc s
    | GetAssertions l -> print_list Term.print oc l
    | GetAssignment l -> print_list print_t_valuation_pair oc l
    | GetInfo l -> print_list print_info oc l
    | GetModel l -> print_list print_model oc l
    | GetOption v -> Attribute.print_value oc v
    | GetProof s -> SExpr.print oc s
    | GetUnsatAssumptions l | GetUnsatCore l -> print_list print_symbol oc l
    | GetValue l -> print_list print_valuation_pair oc l
    | Unsupported -> Format.pp_print_string oc "unsupported"
    | Error s -> Format.fprintf oc "@[(error@ %a)@]" print_string s
end

module Script = struct
  type commands = Command.t list
  type responses = Response.t list

  let print_commands oc l =
    List.iter (Format.fprintf oc "%a@\n" Command.print) l

  let print_responses oc l =
    List.iter (Format.fprintf oc "%a@\n" Response.print) l
end
