# 最適化パス

作成中

α変換を追加します。

## alpha.ml


## main.ml

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Beta.apply e in
  let e' = Assoc.apply e' in
  let e' = Inline.apply e' in
  let e' = ConstFold.apply e' in
  let e' = Elim.apply e' in
  if e = e' then e else
  iter (n - 1) e'

  let k = Alpha.apply(k) in
  fprintf std_formatter "alpha %a@." KNormal.print_t k;


## test.ml

    let k = Alpha.apply(k) in
