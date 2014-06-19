(* helloworld *)
let _ =
  print_string "hello world!\n"

(* 文字列連結 *)
let _ =
  print_string "hello " ^ "world!\n"

(* 変数 *)
let a = 1

(* intを出力 *)
let _ = print_string ("a = " ^ (string_of_int a))

(* let inで繋ぐ *)
let _ =
  let a = 1 in
  print_string ("a = " ^ (string_of_int a) ^ "\n")

(* let inで何個も繋ぐ *)
let _ =
  let a = 1 in
  let b = 2 in
  let c = a + b in
  print_string ("c = " ^ (string_of_int c) ^ "\n")

(* セミコロンで繋ぐ *)

let _ =
  print_string "hello ";
  print_string "world!\n"

(* セミコロンで何個も繋ぐ *)

let _ =
  print_string "hello ";
  print_string "hello ";
  print_string "hello ";
  print_string "hello ";
  print_string "world!\n"

(* let inをネストする *)
let _ =
  let a =
    let x = 1 in
    let y = 2 in
    x + y
  in
  print_string ("a = " ^ (string_of_int a) ^ "\n")

(* セミコロンとlet inの組み合わせ *)
let _ =
  let _ =
    let x = 1 in
    print_string ("x = " ^ (string_of_int x) ^ "\n");
    let y = 2 in
    print_string ("y = " ^ (string_of_int y) ^ "\n");
  in
  let b = 2 in
  let c = a + b in
  print_string ("a = " ^ (string_of_int a) ^ "\n");
  print_string ("b = " ^ (string_of_int b) ^ "\n");
  print_string ("c = " ^ (string_of_int c) ^ "\n")

(* トップレベルは ;;で分割できる*)

;;
let a = 1
;;
let a = 1;
;;
let _ =
  hoge = "hoge\n";

let _ = print_string hoge in ()

(* これ、本当はこういう意味 *)

let _ =
  hoge = "hoge\n";
  let _ = print_string hoge
  in ()

(* ;;でぶった切る *)

let hoge = "hoge\n";
;;

let _ = print_string hoge


(* もしくは、;を付けなければ無問題 *)
let hoge = "hoge\n"

let _ = print_string hoge

(* ;; はトップレベル等の宣言を分割するもの

let a = b
open Hoge
include M

のようなのは宣言で式ではない。

*)
