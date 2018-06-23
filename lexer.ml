module Lexer
= struct 

type token = CID of string | VID of string | NUM of string 
              | TO | IS | QUIT | OPEN | EOF | ONE of char
module P = Printf
exception End_of_system

let _ISTREAM = ref stdin

let ch = ref []

let read () = match !ch with
  [] -> input_char !_ISTREAM
  | h::rest -> (ch := rest; h)

let unread c = ch := c::!ch

let lookahead () = 
  try 
    let c = read () in 
    unread c; 
    c 
  with End_of_file -> '$'

let rec integer i =
  let c = lookahead () in
  if (c >= '0' && c <= '9') then
    integer (i^(Char.escaped (read ())))
  else 
    i

and identifier id =
  let c = lookahead () in
  if ((c >= 'a' && c <= 'z') 
    || (c >= 'A' && c <= 'Z') 
    || (c >= '0' && c <= '9') 
    || c == '_') then
    identifier (id^(Char.escaped (read ())))
  else 
    id

and native_token () =
  let c = lookahead () in
  if (c >= 'a' && c <= 'z') then
    let id = identifier "" in
    if (id = "is") then IS
    else if (id = "quit") then QUIT
    else if (id = "open") then OPEN
    else CID (id)
  else if (c >= 'A' && c <= 'Z') then
    VID (identifier "")
  else if (c >= '0' && c <= '9') then 
    NUM (integer "") 
  else if (c == ':') then
    begin
      ignore (read ());
      let sc = lookahead () in
      if (sc == '-') then
        (ignore (read ());TO)
      else
        ONE (c)
    end
  else ONE (read ())

and gettoken () =
  try
    let token = native_token () in
    match token with
      ONE ' ' -> gettoken ()
      | ONE '\t' -> gettoken ()
      | ONE '\n' -> gettoken ()
      | _ -> token
  with End_of_file -> EOF

and token_string tk = match tk with
  (CID i) -> P.sprintf "CID(%s)" i
  | (VID i) -> P.sprintf "VID(%s)" i
  | (NUM i) -> P.sprintf "NUM(%s)" i
  | (TO) -> P.sprintf ":-"
  | (QUIT) -> P.sprintf "quit"
  | (OPEN) -> P.sprintf "open"
  | (IS) -> P.sprintf "is"
  | (EOF) -> P.sprintf "eof"
  | (ONE c) -> P.sprintf "ONE(%c)" c

and print_token tk = print_string ((token_string tk) ^ "\n")

end
