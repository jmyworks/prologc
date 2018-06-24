module Ast 
= struct 

type ast = Atom of string | Var of string | App of string * (ast list)

end
