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
      if !check then Printf.printf "error expected %s but result %s\n" expected r else ()
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
    if !check then Printf.printf "test_error error \n" else ()
  with
    _ ->
      ok := !ok + 1

let test(src, expected) =
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

  test(Let(
    Print(Int(1)),
    Let(
      Print(Add(Int(2), Int(3))),
      Print(Sub(Add(Int(2), Int(3)), Int(2))))),"(1\n5\n3\n,,0)");
  Printf.printf "test all %d ok %d ng %d\n" !count !ok (!count - !ok)
