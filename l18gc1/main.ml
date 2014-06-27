open Format
open Utils

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.exps Lexer.token lexbuf

let compile output src =
  let ast = parse src in
  fprintf std_formatter "ast %a@." Ast.print_t ast;
  let ast = Syntax.apply(ast) in
  fprintf std_formatter "syntax %a@." Syntax.print_t ast;
  let ast = Typing.apply(ast) in
  fprintf std_formatter "typing %a@." Syntax.print_t ast;
  let ast = FoldPoly.apply(ast) in
  fprintf std_formatter "foldPoly %a@." Syntax.print_t ast;
  let k = KNormal.apply(ast) in
  fprintf std_formatter "knormal %a@." KNormal.print_t k;
  let c = Closure.apply(k) in
  fprintf std_formatter "closure %a@." Closure.print_prog c;
  let v = Virtual.apply(c) in
  fprintf std_formatter "virtual ok@.";
  Emit.apply output v;
  fprintf std_formatter "emit ok@."

let _ =
  let src = "f():={a#=1 b#= & a; *b=5 print(a)} f()" in
  compile "a.ll" src;
  print_exec("llc a.ll -filetype=obj -o a.o");
  print_exec("clang++ -std=c++11 runtime.cpp a.o");
  print_exec("./a.out")
