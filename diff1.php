<?php

$src = $argv[1];
$dst = $argv[2];

ob_start();
system("diff $src $dst");
$r = ob_get_contents();
ob_end_clean();

$r = preg_replace("/((\n[><].*)+)/", "\n```\$1\n```\n", $r);
$r = preg_replace("/\n[><] /", "\n", $r);
echo $r;
