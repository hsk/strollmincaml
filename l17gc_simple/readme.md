# ガーベジコレクション

ここでは、一度今までのソースを忘れて、簡単なシャドースタックを使った例を作ります。


```
%struct..anon1 = type { i64, i64 }
define void @llmain() gc "shadow-stack" {
entry:
  %..av14 = alloca %struct..anon1*
  %..tmp15 = bitcast %struct..anon1** %..av14 to i8**
  call void @llvm.gcroot(i8** %..tmp15, i8* null)
  %..17 = getelementptr %struct..anon1* null, i32 1
  %..sizeI16 = ptrtoint %struct..anon1* %..17 to i32
  %..ptr18 = call i8* @my_alloc(i32 %..sizeI16)
  %b = bitcast i8* %..ptr18 to %struct..anon1*
  store %struct..anon1* %b, %struct..anon1** %..av14
  %..2 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  store i64 3, i64* %..2
  %..3 = mul i64 3, 5
  %..4 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  store i64 %..3, i64* %..4
  %..5 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  %..6 = load i64* %..5
  call void @print_l(i64 %..6) nounwind ssp
  %..7 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  %..8 = load i64* %..7
  call void @print_l(i64 %..8) nounwind ssp
  %..9 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  %..10 = load i64* %..9
  %..11 = getelementptr inbounds %struct..anon1* %b, i64 0, i32 1
  %..12 = load i64* %..11
  %..13 = add i64 %..10, %..12
  call void @print_l(i64 %..13) nounwind ssp
  ret void
}
@.str = private constant [5 x i8] c"%ld\0A\00"
define void @print_l(i64 %a) nounwind ssp {
entry:
  %a_addr = alloca i64, align 8
  store i64 %a, i64* %a_addr
  %0 = load i64* %a_addr, align 8
  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str, i64 0, i64 0), i64 %0) nounwind
  ret void
}
declare i32 @printf(i8*, ...) nounwind
define i32 @main() {
  call void @llmain()
  call void @gc()
  ret i32 0
}
declare void @llvm.gcroot(i8** %ptrloc, i8* %metadata)
declare i8* @my_alloc(i32 %size)
declare void @gc()
```

## 参考

http://yutopp.hateblo.jp/entry/2013/12/01/000152

http://llvm.org/docs/GarbageCollection.html
