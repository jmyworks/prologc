module Evaluator
= struct

include Ast
open Ast
module P = Printf

exception Compiler_error

let sub name term =
  let rec mapVar ast = match ast with
    (Atom x) -> Atom(x)
    | (Var n) -> if n = name then term else Var n
    | (App(n, terms)) -> App(n, List.map mapVar terms)
  in mapVar

let mgu (a, b) = 
  let rec ut (one, another, unifier) = match (one, another) with
    ([], []) -> (true, unifier)
    | (term::t1, Var(name)::t2) ->
      let r = fun x -> sub name term (unifier x) in
      ut(List.map r t1, List.map r t2, r)
    | (Var(name)::t1, term::t2) ->
      let r = fun x -> sub name term (unifier x) in
      ut(List.map r t1, List.map r t2, r)
    | (Atom(n)::t1, Atom(m)::t2) ->
      if n = m then ut(t1, t2, unifier)
      else (false, unifier)
    | (App(n1, xt1)::t1, App(n2, xt2)::t2) ->
      if n1 = n2 && List.length xt1 = List.length xt2 then
        ut(xt1@t1, xt2@t2, unifier)
      else
        (false, unifier)
    | (_, _) -> (false, unifier)
  in ut ([a], [b], (fun x -> x))

let rec get_vars ast_list = match ast_list with
  (App(_, (_ as arg1)::(_ as arg2)::_))::rest -> 
    (get_vars [arg1])@(get_vars [arg2])@(get_vars rest)
  | (Var var)::rest -> (Var var)::(get_vars rest)
  | _ -> []

let rec map_vars (vars, unifier) =
  begin
    List.fold_left 
      (fun vars_mapping var -> 
        ((var, unifier var)::vars_mapping))
      []
      vars
  end 

let rec print_mapping list = match list with
  (key, value)::rest -> print_string (P.sprintf "\n(%s, %s)" (ast2string key) (ast2string value)); print_mapping rest
  | [] -> (print_string "\nempty\n")

let succeed vars_mapping = 
  begin
    let rec filter_var var mapping = match mapping with
      (Var key, Var value)::rest ->
        if var = key then
          (filter_var value rest)
        else
          (filter_var var rest)
      | (Var key, Atom value)::rest ->
        if var = key then value
        else ((filter_var var rest))
      | (_, _)::rest -> (assert false)
      | [] -> (assert false)
    in let rec filter_vars mapping = match mapping with
      (Var key, _)::rest ->
        if (String.contains key '#') then []
        else (key, filter_var key mapping)::(filter_vars rest)
      | (_, _)::rest -> (assert false)
      | [] -> []
    in (filter_vars vars_mapping)
  end

let rename ver term =
  let rec mapVar ast = match ast with
    (Atom x) -> Atom(x)
    | (Var n) -> Var(n ^ "#" ^ ver)
    | (App(n, terms)) -> App(n, List.map mapVar terms)
  in mapVar term

let rec calc expr = match expr with
  App("+", left::right::_) ->
    (calc left) + (calc right)
  | App("-", left::right::_) ->
    (calc left) - (calc right)
  | App("*", left::right::_) ->
    (calc left) * (calc right)
  | App("/", left::right::_) ->
    (calc left) / (calc right)
  | Atom atom ->
    int_of_string atom
  | _ -> raise Compiler_error

let rec solve (program, question, depth, vars_mapping) = match question with
  [] -> [succeed vars_mapping]
  | goal::goals -> match goal with
    App("is", (Var var)::arithmexpr::_) -> 
      [succeed [(Var var, Atom (string_of_int (calc arithmexpr)))]]
    | _ ->
      let onestep results clause = match List.map (rename (string_of_int depth)) clause with
        [] -> raise Compiler_error
        | head::conds ->
          let (unifiable, unifier) = mgu(head, goal) in
            if unifiable then
              (solve (program, List.map unifier (conds@goals), depth+1, 
                vars_mapping@(map_vars ((get_vars question), unifier))))@results
            else
              results
      in List.fold_left onestep [[]] program

let eval (program, question) = 
  begin
    let results = List.filter 
      (fun result -> List.length result > 0) 
      (solve(program, question, 1, [])) in
    let rec remove_dups list = match list with
      [] -> []
      | (key, value)::rest -> 
        (key, value)::(remove_dups (List.filter (fun (k, v) -> k <> key) rest))
    in (List.map remove_dups results)
  end


end
