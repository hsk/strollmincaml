<?php

$main = file_get_contents("main.ml");

$main = preg_replace_callback("/module *([^ ]+) *= *struct\n+(.*?)\n+end\n/s",function($m) {
  $filename = preg_replace_callback("/^./",function($m) {
    return strtolower($m[0]);
  },$m[1].".ml");
  $c = preg_replace("/^  /m", "", $m[2]);
  file_put_contents($filename, $c."\n");
  return "";
}, $main);

$main = preg_replace("/\n\n(\n+)/","\n\n", $main);
echo $main;

