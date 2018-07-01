module Ast 
= struct 

module P = Printf

type ast = Atom of string | Var of string | App of string * (ast list)

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
