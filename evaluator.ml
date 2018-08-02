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
  (App(_, (_ as arg1)::(_ as arg2)::_))::rest -> (get_vars [arg1])@(get_vars [arg2])@(get_vars rest)
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
    in 
    print_string (List.fold_left 
      (fun sentence (name, value) -> 
        P.sprintf "%s%s = %s\n" 
          sentence 
          name 
          value)
      ""
      (filter_vars vars_mapping));
    true
  end

let rename ver term =
  let rec mapVar ast = match ast with
    (Atom x) -> Atom(x)
    | (Var n) -> Var(n ^ "#" ^ ver)
    | (App(n, terms)) -> App(n, List.map mapVar terms)
  in mapVar term

let rec solve (program, question, depth, vars_mapping) = match question with
  [] -> succeed vars_mapping
  | goal::goals ->
    let onestep _ clause = match List.map (rename (string_of_int depth)) clause with
      [] -> raise Compiler_error
      | head::conds ->
        let (unifiable, unifier) = mgu(head, goal) in
          if unifiable then
            let unified_question = (List.map unifier (conds@goals)) in
            solve (program, unified_question, depth+1, vars_mapping@(map_vars ((get_vars question), unifier)))
          else
            true
    in List.fold_left onestep true program

let eval (program, question) = solve(program, [question], 1, [])


end
