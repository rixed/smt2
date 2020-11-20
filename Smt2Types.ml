type symbol = string
type keyword = string
type numeral = string

let symbol_of_string s = s
let numeral_of_int n = string_of_int n
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
  Format.fprintf oc "%s" n

(* When the first of the pair is a keyword: *)
let print_pair p2 oc k v =
  Format.fprintf oc "%a@ %a"
    print_keyword k
    p2 v

module SpecialConstant = struct
  type t =
    Numeral of numeral | Decimal of string | Hexadecimal of string |
    Binary of string | String of string

  let numeral_of_int v = Numeral (string_of_int v)
  let decimal_of_int v = Decimal (string_of_int v)
  let decimal_of_float v = Decimal (Printf.sprintf "%f" v)

  let print oc = function
      Numeral s -> print_numeral oc s
    | Decimal s -> Format.fprintf oc "%s" s
    | Hexadecimal s -> Format.fprintf oc "#x%s" s
    | Binary s -> Format.fprintf oc "#b%s" s
    | String s -> print_string oc s
end

module SExpr = struct
  type t =
      SpecConstant of SpecialConstant.t
    | Symbol of symbol
    | Keyword of keyword
    | List of t list
    | Comment of string (* Not parsed but pretty printed *)

  let comment s = Comment s

  let rec print oc = function
      SpecConstant s -> SpecialConstant.print oc s
    | Symbol s -> Format.fprintf oc "%a" print_symbol s
    | Keyword k -> Format.fprintf oc "%a" print_keyword k
    | List ss -> print_list print oc ss
    | Comment s -> Format.fprintf oc "@[<h>; %s@]@\n" (strip_newlines s)
end

module Identifier = struct
  type index = Numeral of string | Symbol of symbol
  type t = { symbol : symbol ; indices : index list }

  let print_index oc = function
      Numeral s -> Format.fprintf oc "%s" s
    | Symbol s  -> print_symbol oc s

  let print oc = function
      { symbol ; indices = [] } ->
        Format.fprintf oc "%s" symbol
    | { symbol ; indices } ->
        Format.fprintf oc "@[<hov 2>(_@ %s@ %a)@]"
          symbol
          (print_list_content print_index) indices
end

module Attribute = struct
  type value =
    SpecConstant of SpecialConstant.t | Symbol of symbol | SExprs of SExpr.t list
  type t = { keyword : keyword ; value : value option }

  let print_value oc = function
    | SpecConstant s -> SpecialConstant.print oc s
    | Symbol s -> print_symbol oc s
    | SExprs l -> print_list SExpr.print oc l

  let print oc = function
      { keyword ; value = None } ->
        Format.fprintf oc ":%s" keyword
    | { keyword ; value = Some v } ->
        Format.fprintf oc ":%s@ %a" keyword print_value v
end

module Sort = struct
  type t = { identifier : Identifier.t ; sorts : t list }

  let rec print oc = function
      { identifier ; sorts = [] } ->
        Identifier.print oc identifier
    | { identifier ; sorts } ->
        Format.fprintf oc "@[(%a@ %a)@["
          Identifier.print identifier
          (print_list_content print) sorts
end

module Term = struct
  type t =
      SpecConstant of SpecialConstant.t
    | QualIdentifier of identifier
    | Apply of { identifier : identifier ; terms : t list }
    | Let of { bindings : binding list ; term : t }
    | ForAll of { vars : sorted_var list ; term : t }
    | Exists of { vars : sorted_var list ; term : t }
    | Match of { term : t ; cases : match_case list }
    | Annotation of { term : t ; attributes : Attribute.t list }
  and identifier = { identifier : Identifier.t ; sort_opt : Sort.t option }
  and binding = { symbol : symbol ; value : t }
  and sorted_var = { name : symbol ; sort : Sort.t }
  and match_case = { pattern : symbol list ; term : t }

  let print_identifier oc = function
      { identifier ; sort_opt = None } ->
        Identifier.print oc identifier
    | { identifier ; sort_opt = Some sort } ->
        Format.fprintf oc "@[(as@ %a@ %a)@]"
          Identifier.print identifier
          Sort.print sort

  let print_sorted_var oc v =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_symbol v.name
      Sort.print v.sort

  let rec print_binding oc b =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_symbol b.symbol
      print b.value

  and print_pattern oc = function
      [ x ] -> print_symbol oc x
    | l -> print_list print_symbol oc l

  and print_case oc c =
    Format.fprintf oc "@[(%a@ %a)@]"
      print_pattern c.pattern
      print c.term

  and print oc = function
      SpecConstant s -> SpecialConstant.print oc s
    | QualIdentifier i -> print_identifier oc i
    | Apply { identifier ; terms } ->
        Format.fprintf oc "@[(%a@ %a)@]"
          print_identifier identifier
          (print_list print) terms
    | Let { bindings ; term } ->
        Format.fprintf oc "@[(let@ %a@ %a)@]"
          (print_list print_binding) bindings
          print term
    | ForAll { vars ; term } ->
        Format.fprintf oc "@[(forall@ %a@ %a)@]"
          (print_list print_sorted_var) vars
          print term
    | Exists { vars ; term } ->
        Format.fprintf oc "@[(exists@ %a@ %a)@]"
          (print_list print_sorted_var) vars
          print term
    | Match { term ; cases } ->
        Format.fprintf oc "@[(match@ %a@ %a)@]"
          print term
          (print_list print_case) cases
    | Annotation { term ; attributes } ->
        Format.fprintf oc "@[(!@ %a@ %a)@]"
          print term
          (print_list Attribute.print) attributes
end

module Command = struct
  type option =
    | StringOption of keyword * string
    | BoolOption of keyword * bool
    | NumOption of keyword * numeral
    | AttributeOption of Attribute.t

  type sort_dec = { name : symbol ; arity : numeral }

  type selector_dec = { name : symbol ; sort : Sort.t }

  type constructor_dec = { name : symbol ; selectors : selector_dec list }

  type datatype_dec =
      NonParametric of constructor_dec list
    | Parametric of
        { parameters : symbol list ; constructors : constructor_dec list }

  type function_dec =
    { name : symbol ; inputs : Term.sorted_var list ; output : Sort.t }

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
    | SetInfo of Attribute.t
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
    | AttributeResp of Attribute.t

  type sat = Sat | Unsat | Unknown

  type t =
      Success
    | CheckSat of sat
    | Echo of string
    | GetAssertions of Term.t list
    | GetAssignment of (symbol * bool) list
    | GetInfo of info list
    | GetModel of model list
    | GetOption of Attribute.value
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
