<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * Copyright 2009 - 2012 Richard Ash, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "nyquist";
  $pageTitle = _("Nyquist");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<ol>
	<li><b><?=_("Introduction to Nyquist and Lisp Programming")?></b></li>
	<li><a href="nyquist2"><?=_("Programming in Nyquist")?></a></li>
	<li><a href="nyquist3"><?=_("Creating Nyquist Plug-ins")?></a></li>
</ol>

<?php
printf('<p>%s</p>', _('Beginning with version 1.1.1, Audacity allows you to use the Nyquist programming language to write your own plug-in effects for Audacity.  Unlike VST and LADSPA plug-ins, Nyquist plug-ins can be written using an ordinary text editor and don\'t need to be compiled.'));
// i18n-hint: All the %s sequences become HTML tags to make the name 
// "Roger B. Dannenberg" into a link to his home page. So we need the same %s
// in the translated strings for it to work, but there will be no visible text
// inserted in those places.
printf(_('%sNyquist was written by %sRoger B. Dannenberg%s and was intended to be used as a complete programming language for audio synthesis and analysis, with support for MIDI, audio recording and playback, file I/O, object-oriented programming, profiling, debugging and more.  Audacity uses only a subset of Nyquist\'s functionality, allowing you to take simple Nyquist functions and use them to process audio data.   Audacity doesn\'t include any support for debugging Nyquist code, so if you are trying to write a complicated plug-in, you may find it easier to get the full version of Nyquist and develop there, then turn it into an Audacity plug-in.  Nyquist is available from the Carnegie Mellon University Computer Music Project: %sCMU Computer Music Project Software%s - download the full Nyquist here%s'),
	'<p>', '<a href="http://www-2.cs.cmu.edu/~rbd/">', '</a>',
  	'</p><ul><li><a href="http://www-2.cs.cmu.edu/~music/music.software.html">',
   	'</a>', '.</li></ul>' );
// 18n-hint: All The references to Lisp in the strings following are to the programming language.
// So you probably don't want to translate it.
printf(_('%sNyquist supports both a Lisp syntax and a more conventional syntax called SAL. Audacity versions prior to 1.3.8 only support Lisp, but the current Audacity 2.0 series supports both Lisp and SAL. To write plug-ins for use with Audacity, choose the appropriate Nyquist Manual for your version of Audacity and preferred syntax: %sNyquist version 2.37 Manual%s - entirely using Lisp syntax %s Nyquist 3.0x Reference Manual%s - the latest Nyquist Manual (mostly using SAL syntax)%s'), '<p>', '</p><ul><li><a href="http://www.audacity-forum.de/download/edgar/nyquist/nyquist-doc/manual/home.html">', '</a>', '</li><li><a href="http://www.cs.cmu.edu/~rbd/doc/nyquist/">', '</a>', '.</li></ul>');

printf(_('%sNote that you don\'t need to download Nyquist in order to write simple plug-ins to use with Audacity. All the basic instructions you need to work with Lisp and the 2.37 Manual are below. If you want to explore SAL as well, and for the latest Nyquist features in Audacity, see %sNyquist Documentation%s on the %sWiki%s'), '<p>', '<a href="http://wiki.audacityteam.org/wiki/Nyquist_Documentation">', '</a>', '<a href="http://wiki.audacityteam.org/">', '</a>.</p>');

printf('<h3>%s</h3>', _('Lisp'));
printf(_('%sNyquist is based on Lisp.  If you have programmed in Lisp before, you can skim this section or go directly to the %snext page%s.  Otherwise, here\'s an extremely brief introduction to Lisp:'), '<p>', '<a href="nyquist2">', '</a>', '</p>');
printf('<p>%s</p>', _('In Lisp (and therefore Nyquist), everything is an S-Expression, which is just a list of tokens (words) separated by whitespace and enclosed in parentheses.  The name of the function is always the first token in an S-Expression, and all of the other tokens are arguments to this function.  Here\'s a simple example:'));
?>
<pre>
  (setf area (* 3.14159 (expt radius 2)))
</pre>
<?php
// 18n-hint: This one is hard. The first and last %s are HTML and invisible.
// All the others are little code snippets embeded in the text. The examples
// wouldn't make any sense if the snippets were translated, so they aren't
// translatable. The snag is that what is left behind isn't very readable, so
// you might find looking at the final web page
// (// http://audacity.sourceforge.net/help/nyquist) helpful.
printf(_('%sLet\'s break down this example.  The outermost S-expression has three members.  The first one, %s, is the name of the function (it stands for set-field).  %s is used to assign a value to a variable.  (There are other similar functions, like %s and %s, but %s is the most powerful, so it\'s the one we\'ll use in our examples.) After %s comes %s, which is the name of the variable we\'re going to set.  Next comes the value to assign to this variable, which in this case is another S-expression.%s'), 
'<p>', '<code>setf</code>', '<code>setf</code>', '<code>set</code>',
'<code>setq</code>', '<code>setf</code>', '<code>setf</code>',
'<code>area</code>', '</p>' );
printf(_('%sLisp doesn\'t have any special operators for Math functions - they\'re all functions like everything else, using%s prefix %snotation, where the name of the function (or operator) comes before its arguments.  So instead of 3*7 for the product of 3 and 7, in Lisp you would write (* 3 7).  In Nyquist, the %s (exponent) function raises its first argument to the power of the second argument.  Therefore %s means 3.14159 times the square of %s, or the formula for the area of a circle.%s'),
'<p>', '<i>', '</i>', '<code>expt</code>',
'<code>(* 3.14159 (expt radius 2))</code>',
'<code>radius</code>', '</p>');
printf('<p>%s</p>', _('Rather than typing in this full expression every time, let\'s define a function for the area of the circle, that we can call every time we need it:'));
?><pre>
  (defun circlearea (radius) (* 3.14159 (expt radius 2)))
  </pre><?php
printf(_('%sThe %s function is used to define a new function.  The first argument is the name of the function, in this case %s.  The second argument is a list of arguments to the function to be defined - this is one of the few cases where you have an S-expression that is not interpreted as a function call.  Finally the last expression is the value of the function.  Now if we want to compute the area of a circle of radius <code>r</code>, we just need to compute:%s'),
		'<p>','<code>defun</code>','<code>circlearea</code>', '</p>');?>
<pre>
  (setf area (circlearea r))
</pre>
<?php
printf('<p>%s</p>', 
		_('An S-expression is just a representation of a list.  Lisp uses lists to represent just about everything (the name LISP comes from LISt Processing language), so it\'s helpful to know how to manipulate lists.  Let\'s start by assigning a list of numbers to a variable.  You can\'t quite do this:'));

?><pre>
  (setf mylist (1 2 3 4 5))  <font color=#cc0000><--  error!</font>
  </pre><?php

printf(_('%sThe reason this doesn\'t work is that whenever Nyquist sees an S-expression, it tries to evaluate it as a function unless you tell it otherwise.  Since there\'s no function named "1" that takes arguments %s, this will generate an error.  To tell Lisp that you want to treat an S-expression literally, and not to evaluate it as a function, you %squote%s it.  In Nyquist, you can quote a list by putting a single quotation mark before it, like this:%s'),
	   	'<p>', '<code>(2 3 4 5)</code>', '<i>', '</i>','</p>' );

?><pre>
  (setf mylist \'(1 2 3 4 5))
  </pre><?php
printf(_('%sNyquist also provides a %s function that you can use to construct lists - this is useful if some of the elements of the list are functions:%s'),
		'<p>', '<code>list</code>', '</p>');

?><pre>
  (setf mylist (list 1 2 3 4 (sqrt 25)))
</pre><?php
printf(_('%sTo get things off of a list, you can use the %s and %s functions. (Traditionally, these were called %s and %s, respectively, but %s and %s are much easier to remember.  Both sets of names are supported in Nyquist.)  The output of %s is 1, and the output of %s is the list %s.  So the second element of the list is %s.'),
		'<p>', '<code>first</code>','<code>rest</code>', '<code>car</code>',
		'<code>cdr</code>', '<code>first</code>', '<code>rest</code>',
		'<code>(first mylist)</code>', '<code>(rest mylist)</code>',
		'<code>(2 3 4 5)</code>', '<code>(first (rest mylist))</code>',
		'</p>');
printf('<h3>%s</h3>', _('Lisp function reference'));
printf(_('%sHere\'s a list of some of the basic lisp functions you might need.  For a complete list of Lisp / Nyquist functions, see the %sNyquist version 2.37 Reference Manual%s.%s'),
		'<p>', '<a href="http://www.audacity-forum.de/download/edgar/nyquist/nyquist-doc/manual/home.html">',
		'</a>', '</p>');
printf('<p><b>%s</b></p>', _('Note: Symbols in Nyquist (like variable names and function names) are not case-sensitive.  They are converted to uppercase internally.'));

printf('<h4>%s</h4>', _('Math functions'));
printf('<table class="function-list" summary="%s">', 
		// i18n-hint: This is the summary of a table - so not really a sentance
		_('List of functions and explanations'));
printf('<tr><td><code>(+ a b)</code></td><td>%s</td></tr>', _('addition'));
printf('<tr><td><code>(- a b)</code></td><td>%s</td></tr>', _('subtraction'));
printf('<tr><td><code>(* a b)</code></td><td>%s</td></tr>', _('multiplication'));
printf('<tr><td><code>(/ a b)</code></td><td>%s</td></tr>', _('division'));
printf('<tr><td><code>(truncate a b)</code></td><td>%s</td></tr>', _('round down to integer (floor)'));
printf('<tr><td><code>(float a b)</code></td><td>%s</td></tr>', _('integer to floating-point'));
printf('<tr><td><code>(rem a b c ...)</code></td><td>%s</td></tr>', _('remainder'));
printf('<tr><td><code>(min a b c ...)</code></td><td>%s</td></tr>', _('minimum'));
printf('<tr><td><code>(max a b c ...)</code></td><td>%s</td></tr>', _('maximum'));
printf('<tr><td><code>(abs a)</code></td><td>%s</td></tr>', _('absolute value'));
printf('<tr><td><code>(random n)</code></td><td>%s</td></tr>', _('random integer between 1 and n-1'));
printf('<tr><td><code>(sin a b)</code></td><td>%s</td></tr>', _('sine'));
printf('<tr><td><code>(cos a b)</code></td><td>%s</td></tr>', _('cosine'));
printf('<tr><td><code>(tan a b)</code></td><td>%s</td></tr>', _('tangent'));
printf('<tr><td><code>(expt a b)</code></td><td>%s</td></tr>', _('exponent (a to the power of b)'));
printf('<tr><td><code>(sqrt a b)</code></td><td>%s</td></tr>', _('square root'));
printf('<tr><td><code>(&lt; a b)</code></td><td>%s</td></tr>', _('test for a less than b'));
printf('<tr><td><code>(&lt;= a b)</code></td><td>%s</td></tr>', _('test for a less than or equal to b'));
printf('<tr><td><code>(&gt; a b)</code></td><td>%s</td></tr>', _('test for a greater than b'));
printf('<tr><td><code>(&gt;= a b)</code></td><td>%s</td></tr>', _('test for a greater than or equal to b'));
printf('<tr><td><code>(= a b)</code></td><td>%s</td></tr>', _('test for equality'));
printf('<tr><td><code>(/= a b)</code></td><td>%s</td></tr>', _('test for inequality')); ?>
</table>
<?php
printf('<h4>%s</h4>', _('List functions'));
printf('<table class="function-list" summary="%s">',
		_('List of functions and explanations'));
printf('<tr><td><code>(first l)</code></td><td>%s</td></tr>', _('first element of a list (car)'));
printf('<tr><td><code>(rest l)</code></td><td>%s</td></tr>', _('rest of the list (cdr)'));
printf('<tr><td><code>(reverse l)</code></td><td>%s</td></tr>', _('reverse a list'));
printf('<tr><td><code>(list a b ...)</code></td><td>%s</td></tr>', _('construct a list'));
printf('<tr><td><code>(append l1 l2)</code></td><td>%s</td></tr>', _('append two lists'));
printf('<tr><td><code>(length l)</code></td><td>%s</td></tr>', _('length of a list'));
printf('<tr><td><code>(maplist function l)</code></td><td>%s</td></tr></table>', _('apply a function to every element in a list'));

printf('<h4>%s</h4>', _('Control'));
printf('<table class="function-list" summary="%s">', 
		_('List of functions and explanations'));
printf('<tr><td><code>(if expr expr1 expr2)</code></td><td>%s</td></tr></table>', _('if expr is true, evaluates expr1, otherwise evaluates expr2'));

  printf('<h3><a href="nyquist2">%s</a></h3>', _('Next: Programming in Nyquist'));
  include "../include/footer.inc.php";
?>
