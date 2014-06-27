open Main
open Utils

let count = ref 0
let ok = ref 0
let check = ref true

let test_ f src expected =
(*  try*)
    count := !count + 1;
    let r = f src in
    if r <> expected then
      if !check then Printf.printf "error %s expected %s but result %s\n" src expected r else ()
    else
      ok := !ok + 1
(*
  with
  | Error(e) -> Printf.printf "exception %s %s\n" src e
  | _ -> Printf.printf "exception %s\n" src
*)
let test_error f src =
  try
    count := !count + 1;
    let _ = f src in
    if !check then Printf.printf "test_error error %s\n" src else ()
  with
    _ ->
      ok := !ok + 1

let test(src, expected) =
  Printf.printf "compile %s\n" src;
  let f src =
    compile "a.ll" src;
    match exec("llc a.ll -o a.s") with
    | ("","","0") ->
      (match exec("llvm-gcc -m64 a.s") with
      | ("","","0") ->
        (match exec("./a.out") with
        | (a,b,c) ->
          "(" ^ a ^ "," ^ b ^ "," ^ c ^ ")"
        )
      | (a,b,c) ->
        if !check then print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n") else ();
        "llvm-gcc error"
      )
    | (a,b,c) ->
      if !check then print_string("(" ^ a ^ "," ^ b ^ "," ^ c ^ ")\n") else ();
      "llc error"
  in
    test_ f src expected

let check_point name =
  if !check then
    if !count <> !ok then (
      check := false;
      print_string("Let's try stage "^name^"...\n")
    ) else ()
  else ()

open Syntax

let _ =

  test("print(1)print(2 + 3)print((2+3)-2)","(1\n5\n3\n,,0)");
  test("a := 1+2; print(a)","(3\n,,0)");
  test("a := {b := 1+2; b} print(a)","(3\n,,0)");
  test("a := {b := 1+2; print(b); b } print(a)","(3\n3\n,,0)");
  test("
    f(x) := x + 1
    print(f(1))
    print(2 + 3)
    print(2 + 3 - 2)
    a := 1+2
    print(a)
  ", "(2\n5\n3\n3\n,,0)");
  test("f(x):={e(y):=x+y print(1)e}print(f(2)(1))","(1\n3\n,,0)");
  test("f(x):={e(y):=x+y print(e(1))}f(2)","(3\n,,0)");
  test("a := 1 == 1 if(a)print(1)else print(2) if (1==0) print(1) else print(2)","(1\n2\n,,0)");
  test("
    f(a, n) :=
      if (n == 0) a
      else f(a+n, n-1)
    print(f(0, 10))
  ", "(55\n,,0)");
  test("
    fib(n):=
      if (n < 1) 0
      else if (n == 1) 1
      else fib (n - 1) + fib (n - 2)
    print(fib(30))
  ", "(832040\n,,0)");
  test("a := Array.create(2, 112) print(a[1])a[0]=2 print(a[0])","(112\n2\n,,0)");
  test("a := (1,2); (b,c) := a print(b) print(c)", "(1\n2\n,,0)");
  test("a := (1.1,2); (b,c) := a d := b+.2.1-.0.1*.2.0/.1.2 print(c)","(2\n,,0)");
  test("a := (-. 1.1,2); (b,c) := a d := b+.2.1-.0.1*.2.0/.1.2 print(c)","(2\n,,0)");

  test("t:=new 10 print(*t) *t=2 print(*t)","(10\n2\n,,0)");
  test("a #= 1 print(a) a=2 print(a)","(1\n2\n,,0)");


  test("a:=1 print(a)", "(1\n,,0)");
  test("a:=1 b:=2 print(a+b)", "(3\n,,0)");

  check_point "immutable value";


  test("a:= new 1; print(*a)", "(1\n,,0)");
  test("a:= new 1; *a=2; print(*a)", "(2\n,,0)");

  check_point "ptr";

  (* bにポインタコピー *)
  test("a:= new 1; b:=a; print(*b)", "(1\n,,0)");
  test("a:= new 1; b:=a; *a=2; print(*b)", "(2\n,,0)");
  test("a:= new 1; b:=a; *b=2; print(*a)", "(2\n,,0)");

  (* bの入れ物を作成 *)
  test("a:= new 1; b:=new *a; print(*b)", "(1\n,,0)");
  test("a:= new 1; b:=new *a;*a=2; print(*b)", "(1\n,,0)");
  test("a:= new 1; b:=new *a;*b=2; print(*b)", "(2\n,,0)");
  test("a:= new 1; b:=new *a;*b=2; print(*a)", "(1\n,,0)");

  check_point "ptr ptr";

  test("a:= new 1; *a=5; b:=*a; print(b)", "(5\n,,0)");
  test("a:= new 1; b:=2; *a=b; print(*a)", "(2\n,,0)");

  check_point "ptr imm";

(* aがmutableなら aは*aにする *)
(* a#=nは a:=new nである *)
  test("a#= 1; print(a)", "(1\n,,0)");
  test("a#= 1; a=2; print(a)", "(2\n,,0)");

  check_point "ref";

  (* 値を取り出し *)
  test("a#= 1 a=5; b:=a; print(b)", "(5\n,,0)");

  test("a#= 1 b:=a; a=5; print(b)", "(1\n,,0)");
  test("a#= 1 b:=2; a=b; print(a)", "(2\n,,0)");

  check_point "ref imm";

  (* bにポインタコピー *)

  test("a#= 1 b:= &a print(*b)", "(1\n,,0)");
  test("a#= 1 b:= &a  a=2 print(*b)", "(2\n,,0)");
  test("a#= 1 b:= &a;*b=2 print(a)", "(2\n,,0)");

  (* bの入れ物を作成 *)

  test("a#= 1 b:=new a print(*b)", "(1\n,,0)");
  test("a#= 1 b:=new a a=2 print(*b)", "(1\n,,0)");
  test("a#= 1 b:=new a;*b=2 print(*b)", "(2\n,,0)");
  test("a#= 1 b:=new a;*b=2 print(a)", "(1\n,,0)");

  check_point "ref ptr";

  (* bにポインタコピー C++の参照 *)
(*
  test("a#= 1; b#&long= a; b", "(1\n,,0)");
  test("a#= 1; b#&long= a; a=2; b", "(2\n,,0)");
  test("a#= 1; b#&long= a; b=2; a", "(2\n,,0)");
*)
  (* bの入れ物を作成 *)

  test("a#= 1 b#=a print(b)", "(1\n,,0)");
  test("a#= 1 b#=a a=2 print(b)", "(1\n,,0)");
  test("a#= 1 b#=a b=2 print(b)", "(2\n,,0)");
  test("a#= 1 b#=a b=2 print(a)", "(1\n,,0)");

  check_point "ref ref";

  test("a:=new 1 b:= new a print(*a)", "(1\n,,0)");

  test("a:=new 1 b:= new a; *a=5 print(**b)", "(5\n,,0)");
  test("a#=1 b#= & a a=5 print(*b)", "(5\n,,0)");
  test("a#=1 b#= & a; *b=5 print(a)", "(5\n,,0)");


  check_point "ref ref";

  Printf.printf "test all %d ok %d ng %d\n" !count !ok (!count - !ok)
