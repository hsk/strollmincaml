open Format
open Utils

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.exp Lexer.token lexbuf

let compile output src =
  let ast = parse src in
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let ast = Typing.apply(ast) in
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let k = KNormal.apply(ast) in
  fprintf std_formatter "knormal ok@.";
  let c = Closure.apply(k) in
  fprintf std_formatter "closure ok@.";
  let v = Virtual.apply(c) in
  fprintf std_formatter "virtual ok@.";
  Emit.apply output v;
  fprintf std_formatter "emit ok@."

let _ =
  let src = "let rec f x = x+1 in print (f 1); print (2 + 3);print ((2+3)-2); let a = 1+2 in print a" in
  compile "a.ll" src;
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
