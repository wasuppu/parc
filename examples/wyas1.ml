open Parc
(* type lispVal = String of string *)

type lispVal = Atom of string
              | String of string
              | Bool of bool
              | Number of int
              | List of lispVal list
              | DottedList of lispVal list * lispVal

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
          (* choice will consume input even if it fails *)
          (* <|> (char '(' *> (parseList <|> parseDottedList) <* char ')') *)
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

let rec showVal = function
  | String(content) -> "\"" ^  content ^ "\""
  | Atom(name) -> name
  | Number(n) -> string_of_int n
  | Bool(true) -> "#t"
  | Bool(false) -> "#f"
  | List(contents) -> "("  ^ String.concat " " (List.map showVal contents) ^ ")"
  | DottedList(head, tail) -> "(" ^ String.concat " " (List.map showVal head) ^ " . " ^ showVal tail ^ ")"

let rec unpackNum = function
  | Number(n) -> n
  | String(n) -> int_of_string n
  | List([n]) -> unpackNum n
  | _ -> 0

let numericBinop op params =
  match params with
  | [] -> Number(0)
  | x::[] -> x
  | params ->
    let l = (List.map unpackNum params) in
      let calc = function
        | [] -> 0
        | n::ns -> List.fold_left op n ns
    in Number(calc l)

let primitives = [("+", numericBinop (+));
                  ("-", numericBinop (-));
                  ("*", numericBinop ( * ));
                  ("/", numericBinop (/));
                  ("mod", numericBinop ( mod ));]

let apply func args =
  let fopt = List.assoc_opt func primitives in
  match fopt with
  | None -> Bool(false)
  | Some(f) -> f args

let rec eval = function
  | Atom(_) as v -> v
  | String(_) as v -> v
  | Number(_) as v -> v
  | Bool(_) as v -> v
  | List([Atom("quote");v]) -> v
  | List(Atom(func)::args) -> apply func (List.map eval args)
  | _ -> failwith "not implement yet"

let args = ["(- (+ 4 6 3) 3 5)"]

let readExpr input = match run parseExpr input with
  | None -> failwith ("No match: " ^ input)
  | Some(v, _) -> v

let main () =
  let expr =
    match args with
    | [] -> ""
    | expr::_ -> expr
in print_endline @@ showVal @@ eval @@ readExpr expr

let _ = main ()
