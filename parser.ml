include Lexer


module Parser 
= struct
  
  module L = Lexer
  (* module E = Evaluator *)
 
  exception Syntax_error of string
  
  let revTok = ref ([]: L.token list)
  
  let getToken () = match !revTok with 
    [] -> L.gettoken ()
    | h::tl -> (revTok := tl; h)

  let tok = ref (L.ONE ' ')
  
  let revToken t = (revTok := (!tok)::(!revTok); tok := t) 
  
  let advance() = (tok := getToken(); L.print_token (!tok)) 
  
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
    L.EOF -> ()
    | _ -> (clause(); clauses())
  
  and clause() = (terms(); to_opt(); eat(L.ONE '.'))
  
  and to_opt() = match !tok with
    L.TO -> (eat L.TO; terms())
    | _ -> ()
      
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
              clauses(); 
              close_in (!L._ISTREAM)
            end
          | tk -> error(__LINE__, tk)
      end
    | _ -> clauses()
  
  and term() = match !tok with
    L.ONE '(' -> (eat(L.ONE '('); term(); eat(L.ONE ')'))
    | _ -> predicate()
  
  and terms() = (term(); terms'())
  
  and terms'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); term(); terms'())
    | _ -> ()
  
  and predicate() = (funname(); eat(L.ONE '('); args(); eat(L.ONE ')'))
  
  and args() = (expr(); args'())
  
  and args'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); expr(); args'())
    | _ -> ()
  
  and expr() = match !tok with
    L.ONE '(' -> 
      begin
         expr_non_term() 
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
    L.ONE '(' -> (eat(L.ONE '('); expr_non_term(); eat(L.ONE ')'))
    | _ -> (eat(L.ONE '['); list(); eat(L.ONE ']'))
  
  and list() = match !tok with
    L.ONE ']' -> ()
    | _ -> (expr(); list_opt())
  
  and list_opt() = match !tok with
    L.ONE '|' -> (eat(L.ONE '|'); id())
    | L.ONE ',' -> (eat(L.ONE ','); list()) 
    | _ -> ()
  
  and id() = match !tok with
    L.CID _ -> eat(L.CID "")
    | L.VID _ -> eat(L.VID "")
    | _ -> eat(L.NUM "")
 
  and funname() = eat(L.CID "")
  
end

