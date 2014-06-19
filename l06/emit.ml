open Utils
open Virtual

let p(r:r): string =
  match r with
    | RL(_,id) -> "%" ^ id
    | RG(_,id) -> "@" ^ id
    | RN(_,id) -> id

let rec pt(t:Type.t): string =
  match t with
  | Type.Int -> "i64"
  | Type.Unit -> "i32"
  | Type.Fun(ts,t) -> pt t ^ "(" ^ String.concat ", " (List.map pt ts) ^ ")*"
  | Type.Tuple(ts) -> "{" ^ String.concat ", " (List.map pt ts) ^ "}"
  | Type.Var _ -> assert false

let ptr(r:r): string =
  match r with
    | RL(t,_) -> pt t
    | RN(t,_) -> pt t
    | RG(t,_) -> pt t

let pr(r:r): string =
  ptr r ^ " " ^ p r

let emit(v: t) =
  match v with
    | Bin(id, op, a, b) ->
      asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
    | Print(a) ->
      asm_p("call void @print_l(" ^ pr a ^ ") nounwind ssp")

    | Call(id, r, prms) ->
      let ps = String.concat ", " (List.map pr prms) in
      (match regt id with
        | Type.Unit ->
          asm_p("call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
        | _ ->
          asm_p(p(id) ^ " = call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
      )
    | Ret(a) ->
      (match regt a with
        | Type.Unit ->
          asm_p("ret i32 0")
        | _ ->
          asm_p("ret " ^ pr(a))
      )

let apply(file: string) (Prog(fundefs)):unit =
  asm_open(file);

  List.iter (fun
    {name = x; args = args; body = vs; ret = t} ->
    let args = String.concat ", " (List.map (fun (s,t)-> pt t ^ " %" ^ s) args) in
    asm("define "^pt t^" @"^x^"("^args^") nounwind ssp {");
    asm("entry:");
    List.iter (fun v -> emit(v)) vs;
    asm("}");

  ) fundefs;


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
  asm_close()
