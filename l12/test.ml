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
    let ast = parse src in
    let ast = Typing.apply(ast) in
    let k = KNormal.apply(ast) in
    let c = Closure.apply(k) in
    let v = Virtual.apply(c) in
    Emit.apply "a.ll" v;
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
  test("print 1;print (2 + 3);print ((2+3)-2)","(1\n5\n3\n,,0)");
  test("let a = 1+2 in print a","(3\n,,0)");
  test("let a = let b = 1+2 in b in print a","(3\n,,0)");
  test("let a = let b = 1+2 in print b; b in print a","(3\n3\n,,0)");
  test("
    let rec f x =
      x+1
    in print (f 1);
    print (2 + 3);
    print ((2+3)-2);
    let a = 1+2 in
    print a
  ", "(2\n5\n3\n3\n,,0)");
  test("let rec f x = let rec e y = x + y in print 1; e in print ((f 2) 1)","(1\n3\n,,0)");
  test("let rec f x = let rec e y = x + y in print (e 1) in f 2","(3\n,,0)");
  test("let a = 1 = 1 in if a then print 1 else print 2;if 1=0 then print 1 else print 2","(1\n2\n,,0)");
  test("
    let rec f a n =
      if n = 0 then a
      else f (a+n) (n-1)
    in print(f 0 10)
  ", "(55\n,,0)");
  test("
    let rec fib n =
      if n < 1 then 0
      else if n = 1 then 1
      else fib (n - 1) + fib (n - 2)
    in print(fib 30)
  ", "(832040\n,,0)");
  test("let a = Array.create 2 112 in print(a.(1));a.(0)<-2;print(a.(0))","(112\n2\n,,0)");
  test("let a = (1,2) in let (b,c) = a in print(b); print(c)", "(1\n2\n,,0)");
  test("let a = (1.1,2) in let (b,c) = a in let d = b+.2.1-.0.1*.2.0/.1.2 in print(c)","(2\n,,0)");
  test("let a = (-. 1.1,2) in let (b,c) = a in let d = b+.2.1-.0.1*.2.0/.1.2 in print(c)","(2\n,,0)");

  Printf.printf "test all %d ok %d ng %d\n" !count !ok (!count - !ok)

