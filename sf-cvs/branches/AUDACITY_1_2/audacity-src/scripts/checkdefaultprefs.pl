#!/usr/bin/env perl
$matchstr=`grep -r 'gPrefs->Read' ../src`;
@matches = split('\n', $matchstr);
foreach $m (@matches) {
	 if ($m =~ '../src/(.*):.*gPrefs->Read.*\(.*"([^\"]+)", (.+)\);') {
		  push @lines, "$2 $3 ($1)\n";
	 }
}

foreach $l (sort @lines) {
	 print "$l";
}
