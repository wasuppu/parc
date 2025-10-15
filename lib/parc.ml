
(* helper function *)
let s2cl s = List.init (String.length s) (String.get s)
let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let const x = fun _ -> x
let id x = x
let neg x = -x

let compose f g x = f (g x)
let ( << ) = compose

let is_upper = function 'A' .. 'Z' -> true | _ -> false
let is_lower = function 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

(* Parser Monad *)
type 'a parser = P of {parse: string -> ('a * string) option}

let run (P p) inp = p.parse inp

let return x =
  let parse inp =
    Some (x, inp)
  in P {parse = parse}

let bind p f =
  let parse inp =
    match run p inp with
    | None -> None
    | Some(x, inp') -> run (f x) inp'
  in P {parse = parse}

let ( >>= ) = bind
let ( >> ) p1 p2 = p1 >>= const p2
let ( let* ) = ( >>= )

let apply fp p =
    fp >>= (fun f ->
    p >>= (fun x ->
        return (f x)))

let ( <*> ) = apply

let fmap f p = p >>= (fun x -> return (f x))
let ( <$> ) = fmap

let lift2 f p1 p2 = f <$> p1 <*> p2

let ( <* ) p1 p2 = lift2 const p1 p2
let ( *> ) p1 p2 = lift2 (const id) p1 p2


let a b = const id b

(* Parser code *)

(* Standard combinators *)
let empty = P {parse = const None}

let eof =
  let parse inp =
    match s2cl inp with
    | [] -> Some("", "")
    | _::_ -> None
  in P {parse=parse}

let get =
  let parse inp =
    match s2cl inp with
    | [] -> None
    | x::xs -> Some (x, cl2s xs)
  in P {parse = parse}

let filter f p =
  p >>= (fun x ->
    if f x then return x else empty)

let sat f = filter f get

let between start p2 over = start *> p2 <* over

let seq p1 p2 =
  p1 >>= (fun x ->
    p2 >>= (fun y ->
      return (x, y)))

let ( <^> ) = seq

let choice p1 p2 =
  let first_some x y =
    match x with
    | Some _ -> x
    | None -> y
  in
  let parse inp =
    first_some (run p1 inp) (run p2 inp)
  in P {parse = parse}

let ( <|> ) = choice

let choices x = List.fold_left ( <|> ) empty x

let rec many p =
  (p >>= (fun x ->
  many p >>= (fun xs ->
  return (x::xs)))) <|> (return [])

let many1 p =
  p >>= (fun x ->
  many p >>= (fun xs ->
  return (x::xs)))

let manycs cp = cl2s <$> many cp
let manycs1 cp = cl2s <$> many1 cp

let sepby1 p sep = List.cons <$> p <*> many (sep *> p)
let sepby p sep = sepby1 p sep <|> return []
let endby p sep = many (p <* sep)


let chainl1 p pop =
  let comb x (op, y) = op x y in
  let rest = many ((fun x y -> (x, y)) <$> pop <*> p) in
  List.fold_left comb <$> p <*> rest

let chainl p pop x = chainl1 p pop <|> return x

(* Standard parsers *)
let char c = sat (Char.equal c)

let rec str s =
  match s2cl s with
  | [] -> return ""
  | c::cs ->
      char c >>
      str @@ cl2s cs >>
      return @@ cl2s (c::cs)

(* let str s =
  let f p c = cl2s <$> ((List.cons <$> char c) <*> (s2cl <$> p)) in
  List.fold_left f (return "") s *)

let oneOf s =  choices @@ List.map (fun c -> char c) (s2cl s)
let noneOf s = sat @@ (fun c -> not (String.contains s c))

let upper = sat is_upper
let lower = sat is_lower
let digit = sat is_digit
let space = sat is_space

let letter = upper <|> lower
let alphanum = letter <|> digit
let spaces = manycs space

let ident = cl2s <$> (List.cons <$> lower <*> many alphanum)
let nat = int_of_string <$> (cl2s <$> many1 digit)
let int = (~-) <$> (char '-' *> nat) <|> nat

let paren p = between (char '(') p (char ')')
let token p = between (many space) p (many space)