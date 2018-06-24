module Parser 
= struct

  include Lexer
  include Ast
  include Evaluator
 
  module L = Lexer
  module A = Ast
  module E = Evaluator
 
  exception Syntax_error of string
  
  let prog = ref [[A.Var ""]]
  let revTok = ref ([]: L.token list)
  
  let getToken () = match !revTok with 
    [] -> L.gettoken ()
    | h::tl -> (revTok := tl; h)

  let tok = ref (L.ONE ' ')
  
  let revToken t = (revTok := (!tok)::(!revTok); tok := t) 
  
  let advance() = tok := getToken() 
  
  let error(line, excepted) = 
    raise (Syntax_error ("line " ^ (Printf.sprintf "%d" line) ^ ": expected of '"  
            ^ (L.token_string excepted) ^ "' but met '" 
            ^ (L.token_string !tok) ^ "'..."))
  
  let check t = match !tok with
    L.CID _ -> if (t = (L.CID "")) then () else error(__LINE__, t)
    | L.VID _ -> if (t = (L.VID "")) then () else error(__LINE__, t)
    | L.NUM _ -> if (t = (L.NUM "")) then () else error(__LINE__, t)
    | tk -> if (tk = t) then () else error(__LINE__, t)
  
  let eat t = (check t; advance())
  
  let rec clauses() = match !tok with
    L.EOF -> []
    | _ -> (clause()::clauses())
  
  and clause() = 
    begin
      let _terms = terms() in
      let _to = to_opt() in 
      eat(L.ONE '.');
      _terms@_to
    end
  
  and to_opt() = match !tok with
    L.TO -> (eat L.TO; terms())
    | _ -> []
      
  and command() = match !tok with
    L.QUIT -> exit 0
    | L.OPEN -> 
      begin
        eat(L.OPEN);
        match !tok with
          L.CID s -> 
            begin
              eat(L.CID ""); 
              check (L.ONE '.');
              L._ISTREAM := open_in (s^".pl");
              advance(); 
              prog := clauses(); 
              close_in (!L._ISTREAM)
            end
          | tk -> error(__LINE__, tk)
      end
    | _ -> 
      begin
        let _term = term() in
        check(L.ONE '.');
        ignore(E.eval(!prog, _term))
      end
  
  and term() = match !tok with
    L.ONE '(' -> 
      begin
        eat(L.ONE '('); 
        let _term = term() in 
        eat(L.ONE ')');
        _term
      end
    | _ -> predicate()
  
  and terms() = (term()::terms'())
  
  and terms'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); term()::terms'())
    | _ -> []
  
  and predicate() = 
    begin
      let _fn = funname() in
      eat(L.ONE '('); 
      let _args = args() in 
      eat(L.ONE ')');
      A.App(_fn, _args)
    end
  
  and args() = (expr()::args'())
  
  and args'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); expr()::args'())
    | _ -> []

  and expr() = match !tok with
    L.ONE '(' -> 
      begin
        try expr_non_term() with
          Syntax_error _ -> 
            begin
              revToken (L.ONE '(');
              term()
            end
      end
    | L.ONE '[' -> expr_non_term()
    | L.VID _ -> id()
    | L.NUM _ -> id()
    | L.CID s -> 
      begin
        try term() with
          Syntax_error _ -> (revToken (L.CID s); id())
      end
    | _ -> term()

  and expr_non_term() = match !tok with
    L.ONE '(' -> 
      begin
        eat(L.ONE '('); 
        let _non_term = expr_non_term() in 
        eat(L.ONE ')');
        _non_term
      end
    | _ -> 
      begin
        eat(L.ONE '['); 
        let _list = list() in 
        eat(L.ONE ']');
        _list
      end
  
  and list() = match !tok with
    L.ONE ']' -> A.Atom "nil"
    | _ -> A.App("cons", [expr(); list_opt()])
  
  and list_opt() = match !tok with
    L.ONE '|' -> (eat(L.ONE '|'); id())
    | L.ONE ',' -> (eat(L.ONE ','); list()) 
    | _ -> A.Atom "nil"
  
  and id() = match !tok with
    L.CID cid -> (eat(L.CID ""); A.Atom cid)
    | L.VID vid -> (eat(L.VID ""); A.Var vid)
    | L.NUM num -> (eat(L.NUM ""); A.Atom num)
    | _ -> error(__LINE__, L.CID "");
 
  and funname() = match !tok with
    L.CID cid -> (eat(L.CID ""); cid)
    | _ -> error(__LINE__, L.CID "")
  
end

