<?php

	$src = "l01";
	$dst = "l02";

	$out = "$dst.tex";


	ob_start();
	system("diff $src $dst");
	$r = ob_get_contents();
	ob_end_clean();

//	$r = file_get_contents("diff2.txt");
	$r = preg_replace("/^\\t/m","",$r);


	$r = preg_replace("/(\d+),(\d+)[c](\d+),(\d+)/","\n\n\$3〜\$4行目\n",$r);
	$r = preg_replace("/(\d+),(\d+)[c](\d+)/","\n\n\$3行目\n",$r);
	$r = preg_replace("/(\d+)[c](\d+),(\d+)/","\n\n\$2〜\$3行目\n",$r);
	$r = preg_replace("/(\d+)[c](\d+)/","\n\n\$2行目\n",$r);

	$r = preg_replace("/(\d+),(\d+)[a](\d+),(\d+)/","\n\n\$3〜\$4行目に追加\n\n\begin{fv}",$r);
	$r = preg_replace("/(\d+),(\d+)[a](\d+)/","\n\n\$3行目に追加\n\n\begin{fv}",$r);
	$r = preg_replace("/(\d+)[a](\d+),(\d+)/","\n\n\$2〜\$3行目に追加\n\n\begin{fv}",$r);
	$r = preg_replace("/(\d+)[a](\d+)/","\n\n\$2行目に追加\n\n\begin{fv}",$r);

	$r = preg_replace("/((\n<.*)+)/","\n変更前\n\begin{fv}\$1\n\\end{fv}", $r);

//	$r = preg_replace("/<.*\n/","",$r);
	$r = preg_replace("/< (.*)\n/","$1\n",$r);
	$r = preg_replace("/\n---/","\n変更後\n\\begin{fv}", $r);

	$r = preg_replace("/((\n>.*)+)/","\$1\n\\end{fv}", $r);
	$r = preg_replace("/\n> /","\n", $r);

	ob_start();
?>
\chapter{タイトル}

\section{ファイルの追加}

例によって，<?php echo $src ?>をコピーして<?php echo $dst ?>を作成する．
\begin{fv}
cp -r <?php echo $src ?> <?php echo $dst ?>
\end{fv}

<?php echo $r ?>

\section{main.ml}

まず <?php echo $src ?> を <?php echo $dst ?> に書き換える．

\begin{fv}
omake
\end{fv}

\begin{fv}
\end{fv}

\section{まとめ}

<?php
	$r = ob_get_contents();
	ob_end_clean();

	file_put_contents($out, $r);

