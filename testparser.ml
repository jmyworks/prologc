include Parser
include Lexer

let rec run() =
  print_string "?- ";
  while true do
    flush stdout;
    Lexer._ISTREAM := stdin;
    Parser.advance();
    Parser.command();
    print_string "\n?- "
  done;;

run();;
