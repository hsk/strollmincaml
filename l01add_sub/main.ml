open Format
open Utils

module Syntax = struct
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Let of t * t
    | Print of t
end

module Virtual = struct

  type r =
    | RL of string
    | RN of string

  type t =
    | Print of r
    | Bin of r * string * r * r

  let vs :t list ref = ref []

  let add (v:t): unit =
    vs := v :: !vs

  let rec visit(e: Syntax.t): r =
    match e with
    | Syntax.Int(i) ->
      RN(""^ string_of_int i)
    | Syntax.Add(e1, e2) ->
      let reg1 = visit(e1) in
      let reg2 = visit(e2) in
      let reg3 = RL(genid("..")) in
      add(Bin(reg3, "add", reg1, reg2));
      reg3
    | Syntax.Sub(e1, e2) ->
      let reg1 = visit(e1) in
      let reg2 = visit(e2) in
      let reg3 = RL(genid("..")) in
      add(Bin(reg3, "sub", reg1, reg2));
      reg3
    | Syntax.Let(a, b) ->
      let _ = visit(a) in
      visit(b)
    | Syntax.Print(e1) ->
      let reg1 = visit(e1) in
      let reg2 = RN("void") in
      add(Print(reg1));
      reg2

  let apply (e: Syntax.t): t list =
    vs := [];
    let _ = visit(e) in
    List.rev !vs

end

module Emit = struct

  open Virtual

  let p(r:r): string =
    match r with
      | RL(id) -> "%" ^ id
      | RN(id) -> id

  let emit(v: t) =
    match v with
      | Bin(id, op, a, b) ->
        asm_p(p(id) ^ " = " ^ op ^ " i64 " ^ p(a) ^ ", " ^ p(b))
      | Print(a) ->
        asm_p("call i64 @print(i64 " ^ p(a) ^ ") nounwind ssp")

  let apply(file: string) (vs: t list):unit =
    asm_open(file);

    asm("define i64 @main() nounwind ssp {");
    asm("entry:");
    List.iter (fun v -> emit(v)) vs;
    asm_p("ret i64 0");
    asm("}");

    asm("@.str = private constant [5 x i8] c\"%ld\\0A\\00\"");
    asm("define i64 @print(i64 %a) nounwind ssp {");
    asm("entry:");
    asm_p("%a_addr = alloca i64, align 8");
    asm_p("store i64 %a, i64* %a_addr");
    asm_p("%0 = load i64* %a_addr, align 8");
    asm_p("%1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind");
    asm_p("ret i64 0");
    asm("}");
    asm("declare i32 @printf(i8*, ...) nounwind");
    asm_close()

end

let print_exec cmd =
  match exec cmd with
  | (a,b,c) -> fprintf std_formatter "(%s,%s,%s)@." a b c

let compile output ast =
  let vs = Virtual.apply(ast) in
  Emit.apply output vs

open Syntax

let _ =
  let ast =
    Let(
      Print(Int(1)),
      Let(
        Print(Add(Int(2), Int(3))),
        Print(Sub(Add(Int(2), Int(3)), Int(2))))) in
  compile "a.ll" ast;
  print_exec("llc a.ll -o a.s");
  print_exec("llvm-gcc -m64 a.s");
  print_exec("./a.out")
