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

  let trace msg = () (*ignore(Lexer.print_token !tok;print_string (Printf.sprintf ":%s\n" msg)) *) 

  let check t = match !tok with
    L.CID _ -> if (t = (L.CID "")) then () else error(__LINE__, t)
    | L.VID _ -> if (t = (L.VID "")) then () else error(__LINE__, t)
    | L.NUM _ -> if (t = (L.NUM "")) then () else error(__LINE__, t)
    | tk -> if (tk = t) then () else error(__LINE__, t)
  
  let eat t = (check t; advance())
  
  let rec clauses() = trace "clauses";match !tok with
    L.EOF -> []
    | _ -> 
      begin
        let _clause = clause() in
        let _clauses = clauses() in
        _clause::_clauses
      end
  
  and clause() = trace "clause"; 
    begin
      let _terms = terms() in
      let _to = to_opt() in 
      eat(L.ONE '.');
      _terms@_to
    end
  
  and to_opt() = trace "to";match !tok with
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
  
  and term() = trace "term";match !tok with
    L.ONE '(' -> 
      begin
        eat(L.ONE '('); 
        let _term = term() in 
        eat(L.ONE ')');
        _term
      end
    | _ -> predicate()
  
  and terms() = trace "terms";
    begin
      let _term = term() in
      let _terms' = terms'() in
      _term::_terms'
    end
  
  and terms'() = trace "terms'";match !tok with
    L.ONE ',' -> 
      begin
        eat(L.ONE ','); 
        let _term = term() in
        let _terms' = terms'() in
        _term::_terms'
      end
    | _ -> []
  
  and predicate() = trace "predicate";
    begin
      let _fn = funname() in
      eat(L.ONE '('); 
      let _args = args() in 
      eat(L.ONE ')');
      A.App(_fn, _args)
    end
  
  and args() = trace "args";
    begin
      let _expr = expr() in 
      let _args' = args'() in
      _expr::_args'
    end
  
  and args'() = trace "args'";match !tok with
    L.ONE ',' -> 
      begin
        eat(L.ONE ','); 
        let _expr = expr() in
        let _args' = args'() in
        _expr::_args'
      end
    | _ -> []

  and expr() = trace "expr";match !tok with
    L.ONE '(' -> 
      begin
        trace "try expr_non_term";
        try expr_non_term() with
          Syntax_error _ -> trace "try expr_non_term failed";
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
        trace "try term";
        try term() with
          Syntax_error _ -> trace "try term failed";(revToken (L.CID s); id())
      end
    | _ -> term()

  and expr_non_term() = trace "expr_non_term";match !tok with
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
  
  and id() = trace "id";match !tok with
    L.CID cid -> (eat(L.CID ""); A.Atom cid)
    | L.VID vid -> (eat(L.VID ""); A.Var vid)
    | L.NUM num -> (eat(L.NUM ""); A.Atom num)
    | _ -> error(__LINE__, L.CID "");
 
  and funname() = trace "funname";match !tok with
    L.CID cid -> (eat(L.CID ""); cid)
    | _ -> error(__LINE__, L.CID "")
  
end

