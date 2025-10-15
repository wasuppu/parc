open Parc

module EnvMap = Map.Make(String)

type lispVal = Atom of string
              | String of string
              | Bool of bool
              | Number of int
              | List of lispVal list
              | DottedList of lispVal list * lispVal
              | PrimitiveFunc of (lispVal list -> lispVal)
              | Func of {
                  params: string list;
                  varargs: string option;
                  body: lispVal list;
                  closure: envt
                }


and envt = (lispVal ref) EnvMap.t ref

let rec showVal = function
  | String(content) -> "\"" ^  content ^ "\""
  | Atom(name) -> name
  | Number(n) -> string_of_int n
  | Bool(true) -> "#t"
  | Bool(false) -> "#f"
  | List(contents) -> "("  ^ unwordList contents ^ ")"
  | DottedList(head, tail) -> "(" ^ unwordList head ^ " . " ^ showVal tail ^ ")"
  | PrimitiveFunc(_) -> "<primitive>"
  | Func(fn) ->
      let args = String.concat " " fn.params in
      let varargs = match fn.varargs with
        | Some s -> " . " ^ s
        | None -> ""
      in
       Printf.sprintf "(lambda (%s%s) ...)" args varargs
and
  unwordList l = String.concat " " (List.map showVal l)

type listError =
                | UnboundVarErr of string * string
                | NotFunctionErr of string * string
                | BadSpecialFormErr of string * lispVal
                | TypeMismatchErr of string * lispVal
                | NumArgsErr of int * lispVal list
                | ParserErr of string

let showErr = function
  | UnboundVarErr(message, varname) -> message ^ ":" ^ varname
  | NotFunctionErr(message, func) -> message ^ ": " ^ func
  | BadSpecialFormErr(message, form) -> message ^ ": " ^ showVal form
  | TypeMismatchErr(expected, found) -> "Invalid type: expected " ^ expected ^ ", found " ^ showVal found
  | NumArgsErr(expected, found) -> "Expected " ^ string_of_int expected ^ " args; found values " ^ unwordList found
  | ParserErr(message) -> message

let throw e = failwith @@ showErr e

let creatEnv() = (ref EnvMap.empty : envt)

let getVar envr k =
  match EnvMap.find_opt k !envr with
  | Some vr -> !vr
  | None -> throw (UnboundVarErr("Getting unbound var", k))

let setVar envr k v =
  match EnvMap.find_opt k !envr with
  | Some vr -> (vr := v;v)
  | None -> throw (UnboundVarErr("Setting unbound var", k))

let defineVar envr k v = (envr := EnvMap.add k (ref v) !envr; v)

let bindVars envr vars =
  let f (k, v) = envr := EnvMap.add k (ref v) !envr in
  List.iter f vars; envr

let symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

let parseStr = (fun s -> String(s)) <$> (cl2s <$> char '"' *> (many @@ noneOf "\"") <* char '"')

let parseAtom =
  let* first = letter <|> symbol in
  let* rest = many (letter <|> digit <|> symbol) in
  let atom = cl2s (first::rest)
  in
    return (match atom with
          | "true" -> Bool(true)
          | "false" -> Bool(false)
          | _ -> Atom(atom))

let parseNumber = (fun n -> Number(n)) <$> (int_of_string <$> (cl2s <$> (many1 digit)))
let parseNegNum =
  (fun n -> Number(-n)) <$> (int_of_string <$> (cl2s <$> (char '-' *> many1 digit)))

let rec parseExpr =
  let parse inp =
      run (parseStr
          <|> parseNumber
          <|> parseNegNum
          <|> parseAtom
          <|> parseQuoted
          <|> (char '(' *> parseList <* char ')')
          <|> (char '(' *> parseDottedList <* char ')')
          ) inp
  in P{parse=parse}
and parseList =
  let parse inp =
    run (((fun l -> List(l)) <$> (sepby parseExpr spaces))) inp
  in P{parse=parse}
and parseDottedList =
  let parse inp =
    run (
        let* head = endby parseExpr spaces in
        let* tail = (char '.') *> spaces *> parseExpr in
        return (DottedList(head, tail))
        ) inp
  in P{parse=parse}
and parseQuoted =
  let parse inp =
    run (
      char '\'' >>
      let* x = parseExpr in
      return (List([Atom("quote"); x]))
    ) inp
  in P{parse=parse}


let boolBinop unpacker op args =
  if List.length args != 2
  then throw (NumArgsErr(2, args))
  else Bool(op (unpacker @@ List.nth args 0) (unpacker @@ List.nth args 1))


let rec unpackNum = function
  | Number(n) -> n
  | String(n) -> (
        match int_of_string_opt n with
        | None -> throw (TypeMismatchErr("number", String(n)))
        | Some(n) -> n)
  | List([n]) -> unpackNum n
  | notNum -> throw (TypeMismatchErr("number", notNum))

let unpackStr = function
 | String(s) -> s
 | Number(n) -> string_of_int n
 | Bool(b) ->  string_of_bool b
 | notString -> throw (TypeMismatchErr("string", notString))

let unpackBool = function
  | Bool(b) -> b
  | notBool -> throw (TypeMismatchErr("boolean", notBool))

let unpackEquals arg1 arg2 unpacker = unpacker arg1 = unpacker arg2

let numBoolBinop = boolBinop unpackNum
let strBoolBinop = boolBinop unpackStr
let boolBoolBinop = boolBinop unpackBool

let numericBinop op params =
  match params with
  | [] -> throw (NumArgsErr(2, []))
  | x::[] -> throw (NumArgsErr(2, [x]))
  | params ->
    let l = (List.map unpackNum params) in
      let calc = function
        | [] -> 0
        | n::ns -> List.fold_left op n ns
    in Number(calc l)

let car = function
  | [List(x::_)] -> x
  | [DottedList(x::_, _)] -> x
  | [badArg] -> throw (TypeMismatchErr("pair", badArg))
  | badArgList -> throw (NumArgsErr(1, badArgList))

let cdr = function
  | [List(_::xs)] -> List(xs)
  | [DottedList([_], x)] -> x
  | [DottedList(_::xs, x)] -> DottedList(xs, x)
  | [badArg] -> throw (TypeMismatchErr("pair", badArg))
  | badArgList -> throw (NumArgsErr(1, badArgList))

let cons = function
  | [x; List([])] -> List([x])
  | [x; List(xs)] -> List(x::xs)
  | [x; DottedList(xs, xlast)] -> DottedList(x::xs, xlast)
  | [x1; x2] -> DottedList([x1], x2)
  | badArgList -> throw (NumArgsErr(2, badArgList))

let rec eqv = function
  | [Bool(arg1); Bool(arg2)] -> Bool(arg1 = arg2)
  | [Number(arg1); Number(arg2)] -> Bool(arg1 = arg2)
  | [String(arg1); String(arg2)] -> Bool(arg1 = arg2)
  | [Atom(arg1); Atom(arg2)] -> Bool(arg1 = arg2)
  | [List(arg1); List(arg2)] -> (
    let eqvPair (x1, x2) = match eqv [x1;x2] with
      | Bool(b) -> b
      | _ -> false in
    let bs = List.map eqvPair @@ List.combine arg1 arg2 in
    let ands = List.fold_left ( && ) true in
    Bool(List.length arg1 = List.length arg2 && ands bs)
  )
  | [DottedList(xs,x); DottedList(ys,y)] -> eqv [List(xs@[x]); List(ys@[y])]
  | [_;_] -> Bool(false)
  | badArgList -> throw (NumArgsErr(2, badArgList))

let equal =
  let comp arg1 arg2 unpacker =
    try unpackEquals arg1 arg2 unpacker
    with Failure _ -> false
  in
  function
  | [arg1; arg2] -> (
    let eqvEqual =
      match eqv [arg1;arg2] with
        | Bool(x) -> x
        | _ -> false
    in
      let numEqual = comp arg1 arg2 unpackNum in
      let strEqual = comp arg1 arg2 unpackStr in
      let boolEqual = comp arg1 arg2 unpackBool in
      Bool(eqvEqual || numEqual || strEqual || boolEqual)
    )
  | badArgList -> throw (NumArgsErr(2, badArgList))

let primitives = [
                  ("+", numericBinop (+));
                  ("-", numericBinop (-));
                  ("*", numericBinop ( * ));
                  ("/", numericBinop (/));
                  ("mod", numericBinop ( mod ));
                  ("=", numBoolBinop (==));
                  ("<", numBoolBinop (<));
                  (">", numBoolBinop (>));
                  ("!=", numBoolBinop (!=));
                  (">=", numBoolBinop (>=));
                  ("<=", numBoolBinop (<=));
                  ("&&", boolBoolBinop (&&));
                  ("||", boolBoolBinop (||));
                  ("string=?", strBoolBinop (=));
                  ("string<?", strBoolBinop (<));
                  ("string>?", strBoolBinop (>));
                  ("string<=?", strBoolBinop (<=));
                  ("string>=?", strBoolBinop (>=));
                  ("car", car);
                  ("cdr", cdr);
                  ("cons", cons);
                  ("eq?", eqv);
                  ("eqv?", eqv);
                  ("equal?", equal);
                ]


let primitiveBindings envr =
  let makePrimitiveFunc (var, func) = (var, (PrimitiveFunc func)) in
  bindVars envr @@ List.map makePrimitiveFunc primitives


let ( << ) f g x = f (g x)

let makeFunc varargs envr params body = Func {params=List.map showVal params; varargs=varargs; body=body; closure=envr;}
let makeNormalFunc = makeFunc None
let makeVarArgs = makeFunc << (fun x -> Some x) << showVal

let rec drop n lst =
  match n, lst with
  | 0, _ -> lst
  | _, [] -> []
  | _, _::tail -> drop (n - 1) tail

let rec eval envr = function
  | String(_) as v -> v
  | Number(_) as v -> v
  | Bool(_) as v -> v
  | Atom(id) -> getVar envr id
  | List([Atom("quote");v]) -> v
  | List([Atom("if");pred;conseq;alt]) -> (
        match eval envr pred with
          | Bool(false) -> eval envr alt
          | _ -> eval envr conseq
  )
  | List([Atom "set!"; Atom var; form]) -> setVar envr var (eval envr form)
  | List([Atom "define"; Atom(var); form]) -> defineVar envr var (eval envr form)
  | List(Atom("define") :: List(Atom(var) :: params) :: body) -> makeNormalFunc envr params body |> defineVar envr var
  | List(Atom("define") :: DottedList(Atom(var)::params, varrags) :: body) -> makeVarArgs varrags envr params body |> defineVar envr var
  | List(Atom("lambda") :: List(params) :: body) -> makeNormalFunc envr params body
  | List(Atom("lambda") :: DottedList(params, varargs) :: body) -> makeVarArgs varargs envr params body
  | List(Atom("lambda") :: Atom(varargs) :: body) -> makeVarArgs (Atom varargs) envr [] body
  | List(func::args) -> let fn = eval envr func in
                        let argVals = List.map (eval envr) args in
                        apply fn argVals

  | badForm -> throw (BadSpecialFormErr("Unrecognized special form", badForm))
and apply func args = match func with
      | PrimitiveFunc fn -> fn args
      | Func fn ->
          if List.length fn.params != List.length args && fn.varargs = None
          then throw (NumArgsErr((List.length fn.params), args))
          else
            let remaining_args = drop (List.length fn.params) args in
            let evalBody envr =  List.map (eval envr) fn.body |> List.rev |> List.hd in
            let bindVarArgs arg env =
              match arg with
              | Some(argName) -> bindVars env [(argName, List(remaining_args))]
              | None -> env
            in List.combine fn.params args |> bindVars fn.closure |> bindVarArgs fn.varargs  |>  evalBody
      | badform -> throw (NotFunctionErr("Unrecognized function", showVal badform))

let readExpr input = match run parseExpr input with
  | None -> throw (ParserErr("No match: " ^ input))
  | Some(v, _) -> v

let show envr expr = readExpr expr
                      |> eval envr
                      |> showVal
                      |> print_endline

let runOne envr expr = show envr expr

let rec runRepl envr =
  let expr = print_string "> "; read_line() in
  if expr = "quit" then ()
  else
    try
      show envr expr; runRepl envr
    with
      | Failure e -> print_endline e; runRepl envr

let getArgs = List.tl @@ Array.to_list Sys.argv

let main =
  if List.length getArgs == 0 then
    let _ = print_endline "Welcome to Lisp" in
    runRepl (creatEnv() |> primitiveBindings)
  else
    List.hd getArgs |> runOne (creatEnv() |> primitiveBindings)

let _ = main




