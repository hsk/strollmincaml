define i64 @main() gc "shadow-stack" {
entry:
  %..38 = tail call i64 @f..l0() nounwind ssp
  call void @gc()
  ret i64 %..38
}
define i64 @f..l0() gc "shadow-stack" {
entry:
  %..root39 = alloca i8*
  call void @llvm.gcroot(i8** %..root39, i8* null)
  %..28 = call i64* @create_array(i64 1, i64 1) nounwind ssp
  %..root40 = bitcast i64* %..28 to i8*
  store i8* %..root40, i8** %..root39
  %..29 = ptrtoint i64* %..28 to i64
  %..root41 = alloca i8*
  call void @llvm.gcroot(i8** %..root41, i8* null)
  %..30 = call i64* @create_array(i64 1, i64 %..29) nounwind ssp
  %..root42 = bitcast i64* %..30 to i8*
  store i8* %..root42, i8** %..root41
  %..31 = bitcast i64* %..30 to i64**
  %..33 = getelementptr inbounds i64** %..31, i64 0
  %..32 = load i64** %..33
  %..34 = getelementptr inbounds i64* %..32, i64 0
  store i64 5, i64* %..34
  %..36 = getelementptr inbounds i64* %..28, i64 0
  %..35 = load i64* %..36
  %..37 = call i64 @print(i64 %..35) nounwind ssp
  call void @gc()
  ret i64 %..37
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
  %size1 = trunc i64 %size to i32
  %mem1 = call i8* @my_alloc(i32 %size1)
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
declare void @llvm.gcroot(i8** %ptrloc, i8* %metadata)
declare i8* @my_alloc(i32 %size)
declare void @gc()
