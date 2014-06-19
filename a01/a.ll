define i32 @main() nounwind ssp {
entry:
  call void @print_l(i64 1)
  ret i32 0
}
declare void @print_l(i64 %a)
