open Utils
open Virtual

let p(r:r): string =
  match r with
    | RL(_,id) -> "%" ^ id
    | RN(_,id) -> id
    | RG(_,id) -> "@" ^ id

let rec pt(t:Type.t): string =
  match t with
  | Type.Int -> "i64"
  | Type.Bool -> "i1"
  | Type.Unit -> "i64"
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
      (match op with
      | "eq" | "ne" ->
          let reg1 = RL(Type.Bool,genid("..")) in
          asm_p(p(reg1) ^ " = icmp "^ op ^ " " ^ pr(a) ^ ", " ^ p(b));
          asm_p(p(id) ^ " = sext " ^ pr(reg1) ^ " to "^ptr(id));
      | "lt"|"le"|"gt"|"ge"->
          let reg1 = RL(Type.Bool,genid("..")) in
          asm_p(p(reg1) ^ " = icmp s"^ op ^ " " ^ pr(a) ^ ", " ^ p(b));
          asm_p(p(id) ^ " = sext i1 " ^ p(reg1) ^ " to "^ptr(id))
      | _ ->
        asm_p(p(id) ^ " = " ^ op ^ " " ^ pr a ^ ", " ^ p(b))
      )
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
          asm_p("ret i64 0")
        | _ ->
          asm_p("ret " ^ pr(a))
      )
    | InsertValue(r1, r2, r3, i) ->
      asm_p(p(r1) ^ " = insertvalue " ^ pr(r2) ^ ", " ^ pr(r3) ^ ", " ^ string_of_int i)
    | ExtractValue(r1, r2, i) ->
      asm_p(p(r1) ^ " = extractvalue " ^ pr(r2) ^ ", " ^ string_of_int i)
    | Jne(r, label, jmp1, jmp2) ->
      let reg = genid("%reg_") in
      asm_p(reg ^ " = icmp ne " ^ ptr(r) ^ " " ^ p(r) ^ ", 0");
      asm_p("br i1 " ^ reg ^ ", label %" ^ jmp1 ^ ", label %" ^ jmp2);
      asm(label ^ ":")
    | Goto(label, jmp) ->
      asm_p("br label %" ^ jmp);
      if (label <> "") then asm(label ^ ":") else ()
    | Label(jmp, label) ->
      if (jmp <> "") then asm_p("br label %" ^ jmp) else ();
      asm(label ^ ":")
    | Phi(r, l1, l2, t, r1, r2) ->
      asm_p(p(r) ^ " = phi " ^ pt(t) ^ " [" ^ p(r1) ^ ", %" ^ l1 ^ "], [" ^ p(r2) ^ ", %" ^ l2 ^ "]")

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
