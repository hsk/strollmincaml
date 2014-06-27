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
  | Type.Float -> "double"
  | Type.Bool -> "i1"
  | Type.Unit -> "i64"
  | Type.Fun(ts,t) -> pt t ^ "(" ^ String.concat ", " (List.map pt ts) ^ ")*"
  | Type.Array(t) -> pt t ^ "*"
  | Type.Tuple(ts) -> "{" ^ String.concat ", " (List.map pt ts) ^ "}"
  | Type.Var _ -> assert false
  | Type.QVar _ -> assert false

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
    | Call(tail, id, (RG(t,"create_array")as r), prms) ->
      let root1 = genid("..root") in
      let root2 = genid("..root") in
      asm_p("%"^root1^" = alloca i8*");
      asm_p("call void @llvm.gcroot(i8** %"^root1^", i8* null)");

      let tail = if tail then "tail " else "" in
      let ps = String.concat ", " (List.map pr prms) in
      (match regt id with
        | Type.Unit ->
          asm_p(tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
        | _ ->
          asm_p(p(id) ^ " = " ^ tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
      );

      asm_p("%"^root2^" = bitcast "^pr id^" to i8*");
      asm_p("store i8* %"^root2^", i8** %"^root1)

    | Call(tail, id, r, prms) ->





      let tail = if tail then "tail " else "" in
      let ps = String.concat ", " (List.map pr prms) in
      (match regt id with
        | Type.Unit ->
          asm_p(tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
        | _ ->
          asm_p(p(id) ^ " = " ^ tail ^ "call " ^ pr(r) ^ "(" ^ ps ^ ") nounwind ssp")
      )
    | Ret(a) ->
          asm("  call void @gc()");

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
    | Load(reg1, reg2) ->
      asm_p(p(reg1) ^ " = load " ^ pr(reg2))
    | Store(reg1, reg2) ->
      asm_p("store " ^ pr(reg1) ^ ", " ^ pr(reg2))
    | Field(a, b, c) ->
      asm_p(p a ^ " = getelementptr inbounds " ^ pr b ^ ", " ^ pr c)
    | Cast(a, b) ->
      match regt b, regt a with
      | Type.Array(_), Type.Int ->
        asm_p(p a ^ " = ptrtoint " ^ pr b ^ " to " ^ ptr a)
      | Type.Int, Type.Array(_) ->
        asm_p(p a ^ " = inttoptr " ^ pr b ^ " to " ^ ptr a)
      | _,_ ->
        asm_p(p a ^ " = bitcast " ^ pr b ^ " to " ^ ptr a)

let apply(file: string) (Prog(fundefs)):unit =
  asm_open(file);

  List.iter (fun
    {name = x; args = args; body = vs; ret = t} ->
    let args = String.concat ", " (List.map (fun (s,t)-> pt t ^ " %" ^ s) args) in
    asm("define "^pt t^" @"^x^"("^args^") gc \"shadow-stack\" {");
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

  asm("define i64* @create_array(i64 %size, i64 %init) {");
  asm("entry:");
  asm_p("%size1 = trunc i64 %size to i32");
  asm_p("%mem1 = call i8* @my_alloc(i32 %size1)");
  asm_p("%mem = bitcast i8* %mem1 to i64*");
  asm_p("br label %loop");
  asm("");
  asm("loop:");
  asm_p("%i.0 = phi i64 [ %size, %entry ], [ %i.1, %body ]");
  asm_p("%l5 = icmp sgt i64 %i.0, 0");
  asm_p("br i1 %l5, label %body, label %end");
  asm("");
  asm("body:");
  asm_p("%i.1 = sub nsw i64 %i.0, 1");
  asm_p("%addr = getelementptr inbounds i64* %mem, i64 %i.1");
  asm_p("store i64 %init, i64* %addr, align 8");
  asm_p("br label %loop");
  asm("end:");
  asm_p("ret i64* %mem");
  asm("}");

  asm("declare i8* @malloc(i64)");
  asm("declare void @llvm.gcroot(i8** %ptrloc, i8* %metadata)");
  asm("declare i8* @my_alloc(i32 %size)");
  asm("declare void @gc()");

  asm_close()
