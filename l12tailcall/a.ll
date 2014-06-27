define i64 @main() nounwind ssp {
entry:
  %..15 = call i64 @f(i64 0, i64 10) nounwind ssp
  call i64 @print(i64 %..15) nounwind ssp
  ret i64 0
}
define i64 @f(i64 %a, i64 %n) nounwind ssp {
entry:
  %..17 = icmp eq i64 %n, 0
  %..9 = sext i1 %..17 to i64
  %reg_18 = icmp ne i64 %..9, 0
  br i1 %reg_18, label %ok10, label %else11
ok10:
  ret i64 %a
  br label %else11
else11:
  %..12 = add i64 %a, %n
  %..13 = sub i64 %n, 1
  %..14 = tail call i64 @f(i64 %..12, i64 %..13) nounwind ssp
  ret i64 %..14
}
@.str = private constant [5 x i8] c"%ld\0A\00"
define i64 @print(i64 %a) nounwind ssp {
entry:
  %a_addr = alloca i64, align 8
  store i64 %a, i64* %a_addr
  %0 = load i64* %a_addr, align 8
  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind
  ret i64 0
}
declare i32 @printf(i8*, ...) nounwind
define i64* @create_array(i64 %size, i64 %init) {
entry:
  %size1 = mul nsw i64 %size, 8
  %mem1 = call i8* @malloc(i64 %size1)
  %mem = bitcast i8* %mem1 to i64*
  br label %loop

loop:
  %i.0 = phi i64 [ %size, %entry ], [ %i.1, %body ]
  %l5 = icmp sgt i64 %i.0, 0
  br i1 %l5, label %body, label %end

body:
  %i.1 = sub nsw i64 %i.0, 1
  %addr = getelementptr inbounds i64* %mem, i64 %i.1
  store i64 %init, i64* %addr, align 8
  br label %loop
end:
  ret i64* %mem
}
declare i8* @malloc(i64)
