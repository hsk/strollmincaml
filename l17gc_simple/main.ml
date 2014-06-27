open Utils
open Format

module Type = struct
  type t =
    | Unit
    | Int of int
    | Ptr of t
    | Str of (string * t) list

  let rec pp_ls f ppf = function
    | [] -> ()
    | [a] -> fprintf ppf "%a@?" f a
    | x::xs -> fprintf ppf "%a;%a@?" f x (pp_ls f) xs

  let rec pp_t ppf = function
    | Unit -> fprintf ppf "Type.Unit@?"
    | Int(d) -> fprintf ppf "Type.Int(%d)@?" d
    | Ptr(t) -> fprintf ppf "Type.Ptr(%a)@?" pp_t t
    | Str(sts) -> fprintf ppf "Type.Str([%a])@?" pp_sts sts
  and pp_st ppf = function
    | s,t -> fprintf ppf "(\"%s\",%a)@?" s pp_t t
  and pp_sts ppf ls = pp_ls pp_st ppf ls
end

module Syntax = struct
  type t =
    | Long of int
    | Add of t * t
    | Mul of t * t
    | Print of t
    | Block of t list
    | Var of string * Type.t
    | Id of string
    | Assign of t * t
    | Field of string * string
end

module Reg = struct

  type t =
    | RL of Type.t * string
    | RN of Type.t * string
    | RG of Type.t * string

  let pp_r ppf = function
    | RL(t,s) -> fprintf ppf "RL(%a,\"%s\")@?" Type.pp_t t s
    | RN(t,s) -> fprintf ppf "RN(%a,\"%s\")@?" Type.pp_t t s
    | RG(t,s) -> fprintf ppf "RG(%a,\"%s\")@?" Type.pp_t t s

  let pp_tr ppf = function
    | (t,r) -> fprintf ppf "(%a,%a)@?" Type.pp_t t pp_r r

  let pp_trs = Type.pp_ls pp_tr

  let regt (r:t): Type.t =
    match r with
    | RL(t,_) -> t
    | RN(t,_) -> t
    | RG(t,_) -> t

  let regid (r:t): string =
    match r with
    | RL(_,id) -> id
    | RN(_,id) -> id
    | RG(_,id) -> id
end

module Env = struct

  let map :(string * Type.t) list ref = ref []
  let strs :(Type.t * Reg.t) list ref = ref []
  let add (id: string) (t: Type.t): unit =
    map := (id,t)::!map;
    match t with
    | Type.Str(_) ->
      if not (List.mem_assoc t !strs) then
        strs := (t,Reg.RL(t, genid("struct..anon"))) :: !strs;
    | _ -> ()

end

module Virtual = struct
  open Reg

  type t =
    | Print of Reg.t
    | Bin of Reg.t * string * Reg.t * Reg.t
    | Alloca of Reg.t
    | ShadowAlloca of Reg.t
    | Load of Reg.t * Reg.t
    | Store of Reg.t * Reg.t
    | Field of Reg.t * Reg.t * Reg.t * Reg.t
    | BitCast of Reg.t * Reg.t
    | Call of Reg.t * Reg.t * Reg.t list
    | Ret of Reg.t
    | SizeOf of Reg.t * Reg.t
    | Comment of string

  let vs :t list ref = ref []

  let add (v :t) :unit =
    vs := v::!vs

  let index (t:Type.t) (id:string): int =
    match t with
    | Type.Str(ls) ->
      List.fold_left
        (fun n (id1,_) ->
          if id1 = id
          then n
          else n + 1
        )
        0
        ls
    | _ -> failwith "Error"

  let rec arr (e :Syntax.t) :Reg.t =
    match e with
    | Syntax.Id(id) ->
      (try
        RL(Type.Ptr(List.assoc id !Env.map), id)
      with
      | _ -> failwith ("Not found " ^ id)
      )
    | Syntax.Field(id, idx) ->
      (try
        let t = List.assoc id !Env.map in
        let reg1 = RN(Type.Int 32, string_of_int(index t idx)) in
        let reg2 = RN(Type.Int 64, "0") in
        let reg3 = arr(Syntax.Id id) in
        let reg4 = RL(Type.Ptr(Type.Int 64), genid "..") in
        add(Field(reg4, reg3, reg2, reg1));
        reg4
      with
      | _ -> failwith ("Not found "^id)
      )

    | _ -> failwith ("error")

  let rec visit (e: Syntax.t): Reg.t =
    match e with
    | Syntax.Long(i) ->
      RN(Type.Int 64, string_of_int i)
    | Syntax.Add(e1, e2) ->
      let reg1 = visit e1 in
      let reg2 = visit e2 in
      let reg3 = RL(Type.Int 64, genid "..") in
      add(Bin(reg3, "add", reg1, reg2));
      reg3
    | Syntax.Mul(e1, e2) ->
      let reg1 = visit e1 in
      let reg2 = visit e2 in
      let reg3 = RL(Type.Int 64, genid "..") in
      add(Bin(reg3, "mul", reg1, reg2));
      reg3
    | Syntax.Block(es) ->
      List.fold_left
        (fun reg e -> visit e)
        (RN(Type.Unit, "void"))
        es
    | Syntax.Print(e1) ->
      let reg1 = visit e1 in
      let reg2 = RN(Type.Unit, "void") in
      add(Print reg1);
      reg2
    | Syntax.Var(id, t) ->
      let reg1 = RL(Type.Ptr t, id) in
      add(ShadowAlloca reg1);
      fprintf std_formatter "env.add %s %a@." id Type.pp_t t;
      Env.add id t;
      reg1
    | Syntax.Assign(e1, e2) ->
      let reg1 = visit e2 in
      let reg2 = arr e1 in
      add(Store(reg1, reg2));
      reg1
    | Syntax.Id(_) | Syntax.Field(_,_) ->
      let reg1 = arr e in
      let reg2 = RL(Type.Int 64, genid "..") in
      add(Load(reg2, reg1));
      reg2

  let apply (e :Syntax.t) :t list =
    vs := [];
    let _ = visit e in
    List.rev !vs

end

module Emit = struct
  open Reg
  open Virtual

  let p (r:Reg.t): string =
    match r with
    | RL(_, id) -> "%" ^ id
    | RN(_, id) -> id
    | RG(_, id) -> "@" ^ id

  let rec pt (t:Type.t): string =
    match t with
    | Type.Int(i) -> "i" ^ string_of_int i
    | Type.Unit -> "void"
    | Type.Ptr(t) -> pt t ^ "*"
    | Type.Str(_) -> 
      (try
        p(List.assoc t !Env.strs)
      with
      | _ ->
        fprintf std_formatter "env = %a@." pp_trs !Env.strs;
        fprintf str_formatter "Not found %a" Type.pp_t t;
        failwith (flush_str_formatter())
      )

  let ptr (r:Reg.t): string =
    pt (regt r)

  let pr (r:Reg.t): string =
    ptr r ^ " " ^ p r

  let rec add (v: t) =
    match v with
    | Bin(id, op, a, b) ->
      asm_p(p id ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p b)
    | Print(a) ->
      asm_p("call void @print_l(" ^ pr a ^ ") nounwind ssp")
    | Load(reg1, reg2) ->
      asm_p(p reg1 ^ " = load " ^ pr reg2)
    | Store(reg1, reg2) ->
      asm_p("store " ^ pr reg1 ^ ", " ^ pr reg2)
    | Field(reg1, addr, zero, a) ->
      asm_p(p reg1 ^ " = getelementptr inbounds " ^ pr addr ^ ", " ^ pr zero ^ ", " ^ pr a)
    | BitCast(a, b) ->
      asm_p(p a ^ " = bitcast " ^ pr b ^ " to " ^ ptr a)
    | Call(a, b, rs) ->
      if regt a = Type.Unit then
        asm_p("call " ^ pr b ^ "(" ^ String.concat ", " (List.map pr rs) ^ ")")
      else
        asm_p(p a ^ " = call " ^ pr b ^ "(" ^ String.concat ", " (List.map pr rs) ^ ")")
    | Ret(a) ->
      asm_p("ret " ^ pr a)
    | SizeOf(a,b) ->
      let c = RL(Type.Ptr(regt b), genid "..") in
      asm_p(p c ^ " = getelementptr " ^ ptr c ^ " null, i32 1");
      asm_p(p a ^ " = ptrtoint " ^ pr c ^ " to " ^ ptr a)
    | Comment(a) ->
      asm_p("; " ^ a)
    | Alloca(reg) ->
      let t = match regt reg with
        | Type.Ptr(t) -> t
        | _ -> assert false
      in
      asm_p(p reg ^ " = alloca " ^ pt t)
    | ShadowAlloca(reg) ->
      (* 入れ物を用意する *)
      let t = match regt reg with
        | Type.Ptr(t) -> t
        | _ -> assert false
      in
      let av = RL(Type.Ptr(Type.Ptr t), genid "..av") in
      add(Alloca(av));
      let tmp = RL(Type.Ptr(Type.Ptr(Type.Int 8)), genid "..tmp") in
      (* キャストする *)
      add(BitCast(tmp, av));
      (* gcrootを呼ぶ tmpがgcrootに登録される *)
      add(Call(RN(Type.Unit, ""), RG(Type.Unit, "llvm.gcroot"),
        [tmp; RN(Type.Ptr(Type.Int 8), "null")]));
      (* 構造体のサイズを算出する *)
      let sizei = RL(Type.Int 32,genid "..sizeI") in
      add(SizeOf(sizei, RN(t,"")));
      (* アロケータを呼ぶ *)
      let ptr = RL(Type.Ptr(Type.Int 8), genid("..ptr")) in
      add(Call(ptr, RG(Type.Ptr(Type.Int 8),"my_alloc"), [sizei]));
      add(BitCast(reg, ptr));
      (* メモリに保存する *)
      add(Store(reg, av))

  let apply (file: string) (vs: t list) =
    asm_open(file);

    List.iter
      (function
        | (Type.Str(ls), r) ->
          asm(p(r) ^ " = type { " ^
            String.concat ", "
              (List.map (fun(_,t) -> pt t) ls)
            ^ " }")
        | _ -> assert false
      )
      !Env.strs;

    asm("define void @llmain() gc \"shadow-stack\" {");
    asm("entry:");
    List.iter add vs;
    asm_p("ret void");
    asm("}");

    asm("@.str = private constant [5 x i8] c\"%ld\\0A\\00\"");
    asm("define void @print_l(i64 %a) nounwind ssp {");
    asm("entry:");
    asm_p("%a_addr = alloca i64, align 8");
    asm_p("store i64 %a, i64* %a_addr");
    asm_p("%0 = load i64* %a_addr, align 8");
    asm_p("%1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind");
    asm_p("ret void");
    asm("}");
    asm("declare i32 @printf(i8*, ...) nounwind");

    asm("define i32 @main() {");
    asm("  call void @llmain()");
    asm("  call void @gc()");
    asm("  ret i32 0");
    asm("}");
    asm("declare void @llvm.gcroot(i8** %ptrloc, i8* %metadata)");
    asm("declare i8* @my_alloc(i32 %size)");
    asm("declare void @gc()");

    asm_close()

end


let println(a,b,c) =
  Printf.printf "(%s,%s,%s)\n" a b c

open Syntax

let _ =

  let ast = Block([
    Var("b", Type.Str(["x",Type.Int 64]));
    Assign(Field("b", "x"), Long 3);
    Print(Field("b", "x"))
  ]) in
  let codes = Virtual.apply ast in
  Emit.apply "a.ll" codes;
  println(exec("llc a.ll -filetype=obj -o a.o"));
  println(exec("clang++ -std=c++11 runtime.cpp a.o -o a.exe"));
  println(exec("./a.exe"))
