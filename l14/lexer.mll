{
open Parser
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

rule token = parse
| [' ' '\n' '\r' '\t'] { token lexbuf }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "-."           { SUB_DOT }
| "+."           { ADD_DOT }
| "*."           { MUL_DOT }
| "/."           { DIV_DOT }
| '+'            { ADD }
| '-'            { SUB }
| '('            { LPAREN }
| ')'            { RPAREN }
| '{'            { LBRACE }
| '}'            { RBRACE }
| ';'            { SEMICORON }
| ":="           { VALEQ }
| '_'            { IDENT(Utils.genid("..")) }
| ','            { COMMA }
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| '!' { NOT }
| "!=" { NE }
| "==" { EQ }
| "<=" { LE }
| ">=" { GE }
| '<'  { LT }
| '>'  { GT }
| "if" { IF }
| "else" { ELSE }
| "Array.create" { ARRAY_CREATE }
| "new" { NEW }
| '*' { MUL }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '=' { ASSIGN }
| lower (digit|lower|upper|'_')*  { IDENT(Lexing.lexeme lexbuf) }
| eof            { EOF }