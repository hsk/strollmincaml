{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { MINUS }
| '+'
    { PLUS }
| ';'
    { SEMICOLON }
| "print"
    { PRINT }
| "let"
    { LET }
| "in"
    { IN }
| '='
    { EQUAL }
| '_'
    { IDENT(Utils.genid("..")) }
| lower (digit|lower|upper|'_')* 
    { IDENT(Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }
