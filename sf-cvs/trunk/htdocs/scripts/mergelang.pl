#!/usr/bin/perl

if (@ARGV != 1) {
    print "Usage: mergelang.pl lang-code\n";
    exit;
}

$srcfile = "../en/main.inc.php";
$dstfile = "../$ARGV[0]/main.inc.php";
$bakfile = "../$ARGV[0]/main.inc.bak";

open FP, $srcfile;
@l1 = <FP>;
close FP;

if (@l1<=0) {
    print "Could not open \"$srcfile\"\n";
    exit;
}

open FP, $dstfile;
@l2 = <FP>;
close FP;

if (@l2<=0) {
    print "Could not open \"$dstfile\"\n";
    exit;
}

if (-e $bakfile) {
    unlink $bakfile;
}
rename $dstfile, $bakfile;

open FP, ">$dstfile";

foreach $l (@l2) {
    if ($l =~ "\\\$([a-zA-Z0-9]+).*=.*\"(.*)\";") {
	$translated{$1} = $2;
    }
}

foreach $l (@l1) {
    if ($l =~ "\\\$([a-zA-Z0-9]+).*=.*\"(.*)\";") {
	if ($translated{$1}) {
	    print FP "\$$1 = \"$translated{$1}\";\n";
	}
	else {
	    print FP "\$$1 = \"$2\"; /* NEW */\n";
	}
    }
    else {
	print FP "$l";
    }
}
close FP;
