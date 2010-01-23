#!/usr/bin/env perl

open FP, ">all.html";

print FP "<html>\n";
print FP "<head>\n";
print FP "<title>Audacity Nyquist Plug-ins</title>\n";
print FP "</head>\n";
print FP "<body>\n";

@files = split("\n", `ls *.html | sort`);
foreach $f (@files) {
    if ($f =~ "(.*).html" && $f ne "all.html") {
	$base = $1;
	print FP "<a href=\"$1.zip\"><b>$1.zip</b></a><p>";
	open INF, "$f";
	@lines = <INF>;
	foreach $line (@lines) {
	    print FP "$line";
	}
	print FP "<p><hr><p>\n";
    }
}

print FP "</body>\n";
print FP "</html>\n";

