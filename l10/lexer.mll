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
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
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
| "rec"
    { REC }
| "in"
    { IN }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| ','
    { COMMA }
| '_'
    { IDENT(Utils.genid("..")) }
| "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
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
