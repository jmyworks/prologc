module Ast 
= struct 

module P = Printf

type ast = Atom of string | Var of string | App of string * (ast list)
type source = List of ast list | Other of string

let rec source2string source = match source with
  List (_ as list) ->
    begin
      let rec list2string elems = match elems with
        head::rest -> 
          if List.length rest == 0 then
            ast2string head
          else
            (ast2string head) ^ "," ^ (list2string rest)
        | [] -> "[]"
      in P.sprintf "[%s]" (list2string list)
    end
  | Other str -> str

and ast2source ast = match ast with 
  App("cons", (_ as elem)::(Atom "nil")::_) -> 
    List([elem])
  | App("cons", (_ as head)::(App("cons", rest))::_) ->
    begin
      match (ast2source (App("cons", rest))) with
        List (_ as list) -> 
          List(head::list)
        | _ -> assert(false)
    end
  | App("cons", first::second::_) -> 
    Other(P.sprintf "[%s|%s]" (ast2string first) (ast2string second))
  | App("+", (_ as left)::(App(operator, elems))::_) -> 
    Other(P.sprintf "%s + (%s)" (ast2string left) (ast2string (App(operator, elems))))
  | App("-", (_ as left)::(App(operator, elems))::_) -> 
    Other(P.sprintf "%s - (%s)" (ast2string left) (ast2string (App(operator, elems))))
  | App("*", (_ as left)::(App(operator, elems))::_) -> 
    Other(P.sprintf "%s * (%s)" (ast2string left) (ast2string (App(operator, elems))))
  | App("/", [_ as left; App(operator, elems)]) -> 
    Other(P.sprintf "%s / (%s)" (ast2string left) (ast2string (App(operator, elems))))
  | App("+", (_ as left)::(_ as right)::_) -> 
    Other(P.sprintf "%s + %s" (ast2string left) (ast2string right))
  | App("-", (_ as left)::(_ as right)::_) -> 
    Other(P.sprintf "%s - %s" (ast2string left) (ast2string right))
  | App("*", (_ as left)::(_ as right)::_) -> 
    Other(P.sprintf "%s * %s" (ast2string left) (ast2string right))
  | App("/", (_ as left)::(_ as right)::_) -> 
    Other(P.sprintf "%s / %s" (ast2string left) (ast2string right))
  | App(_ as name, (_ as head)::(_ as rest)) ->
    let rec args2string args = match args with
      head::rest ->
        if List.length rest == 0 then
          (ast2string head)
        else
          (ast2string head) ^ "," ^ (args2string rest)
      | [] -> ""
    in (Other(P.sprintf "%s(%s)" name  (args2string (head::rest))))
  | App(_, []) -> assert false
  | Atom (_ as atom) -> Other(atom)
  | Var (_ as var) -> Other(var)

and ast2string ast = source2string (ast2source ast)

let rec print_ast ast = match ast with
  (App(s, hd::tl)) -> 
    begin
      P.printf "App(\"%s\",[" s;
      print_ast hd; 
      List.iter (fun x -> (print_string ";"; print_ast x)) tl;
      print_string "])"
    end
  | (App(s, [])) -> 
    P.printf "App(\"%s\",[])" s | (Atom s) -> P.printf "Atom \"%s\"" s
  | (Var s) -> 
    P.printf "Var \"%s\"" s

let print_ast_list lst = match lst with
  (hd::tl) -> 
    begin
      print_string "["; 
      print_ast hd;
      List.iter (fun x -> (print_string ";";
                  print_ast x)) 
                tl; 
      print_string "]"
    end
  | [] -> print_string "[]"



end
