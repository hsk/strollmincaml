open Format
open Utils

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.exp Lexer.token lexbuf

let _ =
  let src = "let a = (1.1,2) in let (b,c) = a in let d = b+.2.1-.0.1*.2.0/.1.2 in print(c)" in
  let ast = parse src in
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let ast = Typing.apply(ast) in
  fprintf std_formatter "ast=%a@." Syntax.print_t ast;
  let k = KNormal.apply(ast) in
  fprintf std_formatter "knormal %a@." KNormal.print_t k;
  let k = Alpha.apply(k) in
  fprintf std_formatter "alpha %a@." KNormal.print_t k;
  let c = Closure.apply(k) in
  fprintf std_formatter "closure %a@." Closure.print_prog c;
  let v = Virtual.apply(c) in
  fprintf std_formatter "virtual ok@.";
  Emit.apply "a.ll" v;
  fprintf std_formatter "emit ok@.";
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
