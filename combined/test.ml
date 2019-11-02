if true then false else true;;
{$x <- f <- $y};;
{$x <- f <- $y; #y <- $x};;
if true then false else { send $x #y; $x <- $y };;
let m = {close $x } in m;;
{case $x (a => $y <- $z)};;
{work (1); close $y };;
5 + { pay $y (7); close $y };;
{ get $x (5); close $z };;
{ send $x (5+8); $x <- $y };;
{ let x = 5; $x <- $y }
