#!/usr/bin/env perl
chdir "../help/webbrowser";
@helpfiles = glob("*.html");
foreach $hf (@helpfiles) {
	 open FP, "$hf";
	 @lines = <FP>;
	 close FP;
	 foreach $l (@lines) {
		  if ($l =~ '(href|src).?=.?"([^"^#]+)"') {
				$url = $2;
				if ($url =~ 'http') {
					 print "External URL: $url\n";
				}
				elsif ($url =~ 'mailto') {
					 print "Mailto URL: $url\n";
				}
				else {
					 if (!(-e "$url")) {
						  print "### Warning: $url not found in file $hf\n";
					 }
				}				
		  }
		  elsif ($l =~ '(href|src)=([^"][^#^ ]+) ') {
				print "Link w/o quotes: $2 in file $hf\n"
		  }
	 }
}
