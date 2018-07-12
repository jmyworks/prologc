include Lexer

module Parser 
= struct

  module L = Lexer
 
  exception Syntax_error of string
  
  let revTok = ref ([]: L.token list)
  
  let getToken () = match !revTok with 
    [] -> L.gettoken ()
    | h::tl -> (revTok := tl; h)

  let tok = ref (L.ONE ' ')
  
  let revToken t = (revTok := (!tok)::(!revTok); tok := t) 
  
  let advance() = (tok := getToken(); L.print_token !tok)
  
  let error(line, excepted) = 
    raise (Syntax_error (__FILE__ ^ " line " 
      ^ (Printf.sprintf "%d" line) ^ ": expecting '"  
      ^ (L.token_string excepted) ^ "' but met '" 
      ^ (L.token_string !tok) ^ "'"))

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
          L.CID s -> (
              eat(L.CID ""); 
              check (L.ONE '.');
              L._ISTREAM := open_in (s^".pl");
              advance(); 
              clauses(); 
              close_in (!L._ISTREAM))
          | tk -> error(__LINE__, tk)
      end
    | _ -> (terms(); check(L.ONE '.'))
  
  and term() = match !tok with
    L.ONE '(' -> (eat(L.ONE '('); term(); eat(L.ONE ')'))
    | L.VID _ -> (eat(L.VID ""); eat(L.IS); arithmexp())
    | _ -> predicate()
  
  and terms() = (term(); terms'())
  
  and terms'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); term(); terms'())
    | _ -> ()
  
  and predicate() = (
    funname();
    eat(L.ONE '('); 
    args(); 
    eat(L.ONE ')'))
  
  and args() = (expr(); args'())
  
  and args'() = match !tok with
    L.ONE ',' -> (eat(L.ONE ','); expr(); args'())
    | _ -> ()

  and expr() = match !tok with
    L.ONE '(' -> (eat(L.ONE '('); expr(); eat(L.ONE ')'))
    | L.ONE '[' -> (eat(L.ONE '['); list(); eat(L.ONE ']'))
    | L.CID s -> 
      begin
        tok := getToken();
        match
          try (check(L.ONE '('); true) with
            Syntax_error _ -> false
        with true -> (revToken (L.CID s); predicate())
          | false -> (revToken (L.CID s); arithmexp())
      end
    | _ -> arithmexp()
    
  and arithmexp() = (arithmterm(); arithmexp'())

  and arithmexp'() = match !tok with
    L.ONE '+' -> (eat(L.ONE '+'); arithmterm(); arithmexp'())
    | L.ONE '-' -> (eat(L.ONE '-'); arithmterm(); arithmexp'())
    | _ -> ()

  and arithmterm() = (arithmfactor(); arithmterm'())

  and arithmterm'() = match !tok with
    L.ONE '*' -> (eat(L.ONE '*'); arithmfactor(); arithmterm'())
    | L.ONE '/' -> (eat(L.ONE '/'); arithmfactor(); arithmterm'()) 
    | _ -> ()

  and arithmfactor() = match !tok with
    L.ONE '(' -> (eat(L.ONE '('); arithmexp(); eat(L.ONE ')'))
    | L.ONE '-' -> (eat(L.ONE '-'); arithmexp())
    | _ -> id()

  and list() = match !tok with
    L.ONE ']' -> ()
    | _ -> (expr(); list_opt())
  
  and list_opt() = match !tok with
    L.ONE '|' -> (eat(L.ONE '|'); id())
    | L.ONE ',' -> (eat(L.ONE ','); list()) 
    | _ -> ()
  
  and id() = match !tok with
    L.CID cid -> eat(L.CID "")
    | L.VID vid -> eat(L.VID "")
    | L.NUM num -> eat(L.NUM "")
    | _ -> error(__LINE__, L.CID "")
 
  and funname() = match !tok with
    L.CID cid -> eat(L.CID "")
    | _ -> error(__LINE__, L.CID "")
  
end

let rec run() =
  print_string "?- ";
  while true do
    flush stdout; 
    Lexer._ISTREAM := stdin;
    Parser.advance(); 
    Parser.command(); 
    print_string "\n?- "
  done

