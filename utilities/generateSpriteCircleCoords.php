<?php
$xmultiplier = 50;
$ymultiplier = 80;
$xoffset = 120;
$yoffset = 160;
$spriteHeight = 6;

echo "; Sprite coordinates automatically generated.\n";
echo "; VSTART, HSTART, VSTOP\n";

for ($i=0; $i<360; $i++) {
	$x = intval((sin(deg2rad($i))) * $xmultiplier) + $xoffset;
	$y = intval((cos(deg2rad($i))) * $ymultiplier) + $yoffset;
	echo sprintf("\tdc.w\t$%02x%02x,$%02x00\n",$y,$x,$y+$spriteHeight);
}