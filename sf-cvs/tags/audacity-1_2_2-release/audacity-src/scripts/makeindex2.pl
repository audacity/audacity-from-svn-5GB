#!/usr/bin/env perl
print "Indexing items in webbrowser help...\n";
chdir "../help/webbrowser";
@files = glob("*.html");

foreach $f (@files) {
	open IF, "$f";
	@lines = <IF>;
	close IF;
	$anchor = "";
	foreach $l (@lines) {
		 if ($l =~ '<a name="([^"]+)"') {
			  $anchor = $1;
		 }
		 if ($l =~ "<[Pp] class=menuref_item>([A-Z][^\.]*)[\.]*</") {
			  $phrase = $1;
			  add_phrase($phrase, $f, $anchor);
	    }
		 if ($l =~ "<[Pp] class=q[a-z]?>([A-Z][^\.]*)[\.]*</") {
			  $phrase = $1;
			  add_phrase($phrase, $f, $anchor);
	    }

		 if ($l =~ "<.*class=title.*>([A-Z][^\.]*)[\.]*</") {
			  $phrase = $1;
			  add_phrase($phrase, $f, $anchor);
	    }
		 if ($l =~ "<[Bb]>([A-Z][^<]*)</") {
			  $phrase = $1;
			  @words = split(" ", $phrase);
			  if (@words >= 1 &&
					@words <= 5 &&
					$words[0] ne "The" &&
					$words[0] ne "Tip:" &&
					$phrase ne "Audacity") {
					add_phrase($phrase, $f, $anchor);
			  }
	    }
	}
}

@keys = sort(keys %dict);

foreach $d (@keys) {
    $f = $dict{$d};
	 print "<a href=\"$f\">$d</a>\n";
};

sub add_phrase {
	 $phrase = $_[0];
	 $f = $_[1];
	 $anchor = $_[2];

	 if ($dict{$phrase} eq $f ||
		  $dict{$phrase} eq "$f#$anchor") {
		  return;
	 }

	 $phrase_orig = $phrase;
	 $c = 1;
	 while ($dict{$phrase}) {
		  $c++;
		  $phrase = "$phrase_orig ($c)";
	 }

	 if ($anchor) {
		  $dict{$phrase} = "$f#$anchor";
	 }
	 else {
		  $dict{$phrase} = $f;		  
	 }
}

