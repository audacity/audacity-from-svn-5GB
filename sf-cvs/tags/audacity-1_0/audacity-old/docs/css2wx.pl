#!/usr/bin/perl -w
#
# !!start inside the source directory!!
#
# No source files will be overwritten
# IF the $target variable is set properly
#
# Find a starting tag like
# <p class=qy>
# then find the closing tag </p>
# and insert <font ...> tags to encompass the stuff
# inside the <P> tags
#
#
#  Rule:
# START or END tags must not span several lines
# for example :
# <p
# class=qy>
# This wouldn't work anymore
# 
# At VERSION 1, this tool will only search for one pair of markers,
# a file glob will gather all *.html files and write them
# to a directory, defined in the
#    $target   variable.
#
# It's not bulletproof in any way. It cannot handle
# <OPENING>...<CLOSING>...<OPENING>
# ...
# <CLOSING>
#
# I have tried to close every <P> tag properly in the original source
# and have avoided any <P></P><P> lines, so thankfully this works ok.
# Those dumb HTML classes are a mystery to me. I wrote this so much faster,
# but someone may want to implement this little script with HTML::parser
# methods or something simliar.
#

@list = glob("*.html");

$targetdir = "../help/"; # stuff 'em where ?

@tagwork = (	'<p class=menuref_item>',	'</p>',		# 1
		'<font size="+2" color="RED">',	'</font>',	# insert after opening tag & before closing tag

		'<p class=q>',			'</p>',		# 2
		'<font size="+2">',		'</font>',

		'<p class=qr>',			'</p>',		# 3
		'<font size="+2">',		'</font>',

		'<p class=qg>',			'</p>',		# 4
		'<font size="+2">',		'</font>',

		'<p class=qb>',			'</p>',		# 5
		'<font size="+2">',		'</font>',

		'<p class=qy>',			'</p>',		# 6
		'<font size="+2">',		'</font>',

		'<p class=qo>',			'</p>',		# 7
		'<font size="+2">',		'</font>'
	   );

$busttag = '<p';	# closing <P> tags properly isn't all that common
			# so this is checked against too
			# actualy this should be part of the @tagwork array
			# but I wanted to continue writing docs :)

@file = ();
@file_target = ();
%file_errors = ();

# begin file loop

foreach $filename (@list) {
	
    open ( SOURCE, "<$filename" ) or die "Could not read $filename: $!\n";
    @file = <SOURCE> ;
    close (SOURCE);
    chomp @file;

    @file_target = ();

    $tagp = 0;	# initialize the pointer of @tagwork


  TAGCYCLE: # through each bunch of tags to be processed -> 1 pass over the file per substitution
  for ( $tagp=0; $tagp <= $#tagwork; $tagp += 4 ) {

	# $tagwork [
	# $tagp		points to the opening tag we are searching for
	# $tagp + 1	points to the closing tag we are searching for
	# $tagp + 2	points to the opening tag we wish to insert after  the opening tag we have found
	# $tagp + 3	points to the closing tag we wish to insert before the closing tag we have found
	# ]

    STARTCHECK:
    for ( $i=0; $i <= $#file; $i++ ) {



        if ( $file[$i] =~ /$tagwork[$tagp]/i ) {
        	# we found our opening tag
		# but are both the opening and the closing tags
		# on the current line ?
		OPENINGTAG:
		if ( $file[$i] =~ /$tagwork[$tagp].*?$tagwork[$tagp+1]/i ) {

			# then we can do the substitution right here
			$file[$i] =~
			s/($tagwork[$tagp])(.*?)($tagwork[$tagp+1])/$1$tagwork[$tagp+2]$2$tagwork[$tagp+3]$3/i ;
			# and move on to check the next line
			next STARTCHECK;
		}

		# check for the opening AND bust tag (another <P that would end a paragraph)
		if ( $file[$i] =~ /$tagwork[$tagp].*?$busttag/i ) {

			# then we can do the substitution right here
			$file[$i] =~
			s/($tagwork[$tagp])(.*?)($busttag)/$1$tagwork[$tagp+2]$2$tagwork[$tagp+3]$3/i ;
			# and move on to check the next line
			next STARTCHECK;
		}
		# and finally check for the opening AND ANOTHER opening tag
		if ( $file[$i] =~ /$tagwork[$tagp].*?$tagwork[$tagp]/i ) {

			# then we can do the substitution right here, except the starter tag is surrounded
			# by both the end and opening insertion tags
			$file[$i] =~
			s/($tagwork[$tagp])(.*?)($tagwork[$tagp])/$1$tagwork[$tagp+2]$2$tagwork[$tagp+3]$3$tagwork[$tagp+2]/i ;
			# now we simply move on here
		}


		# so it's just the opening tag on this line

        	$i++ ;	# we'll start looking for the closing tag AND a new opening tag(it happens with <P>)
        		# on the next line

        	CLOSINGTAG:
        	for ( $j=$i; $j<=$#file; $j++ ) {

			if ( $file[$j] =~ /$tagwork[$tagp+1]/ ) {

                        # we found the CLOSING tag
                        # lets insert the closing tag from the list
			$file[$i] =~
			s/($tagwork[$tagp+1])/$tagwork[$tagp+3]$1/i ;

			# and set the search to this line. the for loop will ++ it.
                        $i = $j;
                        next STARTCHECK; # check next line for opening tags

                    } elsif ( $file[$i] =~ /$tagwork[$tagp]/i  || $file[$i] =~ /$busttag/i ) {
                        # no closing tag !
                        # so let's startup the check with the current line
                        # the for loop will ++ $i
                        $i = $j - 1;
                        #
                        next STARTCHECK;           # get back to the START marker check.
                    }
                } # end of for loop with $j

	} # end of line check for the START & END marker
	

    } # end of STARTCHECK for loop

  } # end of TAGCYCLE for loop
    # all tags were processed for this file

# ------------- #
	 if ($filename =~ "(.*).html") {
		  $filename = "$1.htm";
	 }
    open ( TARGET, ">$targetdir" . "$filename" );
    for ($i=0;$i<=$#file;$i++){
		  $line = $file[$i];
		  $line =~ s/html/htm;
		  print TARGET $line . "\n";
    }
    close (TARGET)
} # end of foreach file loop

print "\n Thank you for calling\n";
if (keys %file_errors) {
	print "Number of errors encountered in file :\n";
        foreach $key (keys %file_errors) {
	print "  $file_errors{$key} --- $key\n";
	}
}
exit;
