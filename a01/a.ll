define i64 @main() nounwind ssp {
entry:
  call i64 @print(i64 1)
  ret i64 0
}
declare void @print(i64 %a)
