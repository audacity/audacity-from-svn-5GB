<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2003 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
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

<?=_('<p>Beginning with version 1.1.1, Audacity allows you to use the Nyquist programming language to write your own plug-in effects for Audacity.  Unlike VST and LADSPA plug-ins, Nyquist plug-ins can be written using an ordinary text editor and don\'t need to be compiled.</p>

<p>Nyquist was written by <a href="http://www-2.cs.cmu.edu/~rbd/">Roger B. Dannenberg</a> and was intended to be used as a complete programming language for audio synthesis and analysis, with support for MIDI, audio recording and playback, file I/O, object-oriented programming, profiling, debugging, and more.  Audacity uses only a subset of Nyquist\'s functionality, allowing you to take simple Nyquist functions and use them to process audio data.   Audacity doesn\'t include any support for debugging Nyquist code, so if you are trying to write a complicated plug-in, you may find it easier to get the full version of Nyquist and develop there, then turn it into an Audacity plug-in.  Nyquist is available from the Carnegie Mellon University Computer Music Project:</p>
<ul>
	<li><a href="http://www-2.cs.cmu.edu/~music/music.software.html">CMU Computer Music Project Software</a> - download the full Nyquist here</a></li>
	<li><a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Complete Nyquist Reference Manual at CMU</a></li>
</ul>

<p>Note that you don\'t need to download Nyquist in order to write simple plug-ins to use with Audacity.  All of the instructions you need are below.</p>

<h3>Lisp</h3>

<p>Nyquist is based on Lisp.  If you have programmed in Lisp before, you can skim this section or go directly to the <a href="nyquist2">next page</a>.  Otherwise, here\'s an extremely brief introduction to Lisp:</p>

<p>In Lisp (and therefore Nyquist), everything is an S-Expression, which is just a list of tokens (words) separated by whitespace and enclosed in parentheses.  The name of the function is always the first token in an S-Expression, and all of the other tokens are arguments to this function.  Here\'s a simple example:</p>

<pre>
  (setf area (* 3.14159 (expt radius 2)))
</pre>

<p>Let\'s break down this example.  The outermost S-expression has three members.  The first one, <code>setf</code>, is the name of the function (it stands for set-field).  <code>setf</code> is used to assign a value to a variable.  (There are other similar functions, like <code>set</code> and <code>setq</code>, but <code>setf</code> is the most powerful, so it\'s the one we\'ll use in our examples.) After <code>setf</code> comes <code>area</code>, which is the name of the variable we\'re going to set.  Next comes the value to assign to this variable, which in this case is another S-expression.</p>

<p>Lisp doesn\'t have any special operators for Math functions - they\'re all functions like everything else, using <i>prefix</i> notation, where the name of the function (or operator) come before its arguments.  So instead of 3*7 for the product of 3 and 7, in Lisp you would write (* 3 7).  In Nyquist, the <code>expt</code> (exponent) function raises its first argument to the power of the second argument.  Therefore <code>(* 3.14159 (expt radius 2))</code> means 3.14159 times the square of <code>radius</code>, or the formula for the area of a circle.</p>

<p>Rather than typing in this full expression every time, let\'s define a function for the area of the circle, that we can call every time we need it:</p>

<pre>
  (defun circlearea (radius) (* 3.14159 (expt radius 2)))
</pre>

<p>The <code>defun</code> function is used to define a new function.  The first argument is the name of the function, in this case <code>circlearea</code>.  The second argument is a list of arguments to the function to be defined - this is one of the few cases where you have an S-expression that is not interpreted as a function call.  Finally the last expression is the value of the function.  Now if we want to compute the area of a circle of radius <code>r</code>, we just need to compute:</p>

<pre>
  (setf area (circlearea r))
</pre>

<p>An S-expression is just a representation of a list.  Lisp uses lists to represent just about everything (the name LISP comes from LISt Processing language), so it\'s helpful to know how to manipulate lists.  Let\'s start by assigning a list of numbers to a variable.  You can\'t quite do this:</p>

<pre>
  (setf mylist (1 2 3 4 5))  <font color=#cc0000><--  error!</font>
</pre>

<p>The reason this doesn\'t work is that whenever Nyquist sees an S-expression, it tries to evaluate it as a function unless you tell it otherwise.  Since there\'s no function named "1" that takes arguments <code>(2 3 4 5)</code>, this will generate an error.  To tell lisp that you want to treat an S-expression literally, and not to evaluate it as a function, you <i>quote</i> it.  In Nyquist, you can quote a list by putting a single quotation mark before it, like this:</p>

<pre>
  (setf mylist \'(1 2 3 4 5))
</pre>

<p>Nyquist also provides a <code>list</code> function that you can use to construct lists - this is useful if some of the elements of the list are functions:</p>

<pre>
  (setf mylist (list 1 2 3 4 (sqrt 25)))
</pre>

<p>To get things off of a list, you can use the <code>first</code> and <code>rest</code> functions.  (Traditionally, these were called <code>car</code> and <code>cdr</code>, respectively, but <code>first</code> and <code>rest</code> are much easier to remember.  Both sets of names are supported in Nyquist.)  The output of <code>(first mylist)</code> is 1, and the output of <code>(rest mylist)</code> is the list <code>(2 3 4 5)</code>.  So the second element of the list is <code>(first (rest mylist))</code>.</p>

<h3>Lisp function reference</h3>

<p>Here\'s a list of some of the basic lisp functions you might need.  For a complete list of Lisp / Nyquist functions, see the <a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist Reference Manual</a>.</p>

<p><b>Note: Symbols in Nyquist(like variable names and function names) are not case sensitive.  They are converted to uppercase internally.</b></p>

<h4>Math functions</h4>
<table class="function-list" summary="List of functions and explanations">
<tr><td><code>(+ a b)</code></td><td>addition</td></tr>
<tr><td><code>(- a b)</code></td><td>subtraction</td></tr>
<tr><td><code>(* a b)</code></td><td>multiplication</td></tr>
<tr><td><code>(/ a b)</code></td><td>division</td></tr>
<tr><td><code>(truncate a b)</code></td><td>round down to integer (floor)</td></tr>
<tr><td><code>(float a b)</code></td><td>integer to floating-point</td></tr>
<tr><td><code>(rem a b c ...)</code></td><td>remainder</td></tr>
<tr><td><code>(min a b c ...)</code></td><td>minimum</td></tr>
<tr><td><code>(max a b c ...)</code></td><td>maximum </td></tr>
<tr><td><code>(abs a)</code></td><td>absolute value</td></tr>
<tr><td><code>(random n)</code></td><td>random integer between 1 and n-1</td></tr>
<tr><td><code>(sin a b)</code></td><td>sine</td></tr>
<tr><td><code>(cos a b)</code></td><td>cosine</td></tr>
<tr><td><code>(tan a b)</code></td><td>tangent</td></tr>
<tr><td><code>(expt a b)</code></td><td>exponent (a to the power of b)</td></tr>
<tr><td><code>(sqrt a b)</code></td><td>square root</td></tr>
<tr><td><code>(< a b)</code></td><td>test for a less than b</td></tr>
<tr><td><code>(<= a b)</code></td><td>test for a less than or equal to b</td></tr>
<tr><td><code>(> a b)</code></td><td>test for a greater than b</td></tr>
<tr><td><code>(>= a b)</code></td><td>test for a greater than or equal to b</td></tr>
<tr><td><code>(= a b)</code></td><td>test for equality</td></tr>
<tr><td><code>(/= a b)</code></td><td>test for inequality</td></tr>
</table>
<h4>List functions</h4>
<table class="function-list" summary="List of functions and explanations">
<tr><td><code>(first l)</code></td><td>first element of a list (car)</td></tr>
<tr><td><code>(rest l)</code></td><td>rest of the list (cdr)</td></tr>
<tr><td><code>(reverse l)</code></td><td>reverse a list</td></tr>
<tr><td><code>(list a b ...)</code></td><td>construct a list</td></tr>
<tr><td><code>(append l1 l2)</code></td><td>append two lists</td></tr>
<tr><td><code>(length l)</code></td><td>length of a list</td></tr>
<tr><td><code>(maplist function l)</code></td><td>apply a function to every element in a list</td></tr>
</table>

<h4>Control</h4>
<table class="function-list" summary="List of functions and explanations">
<tr><td><code>(if expr expr1 expr2)</code></td><td>if expr is true, evaluates expr1, otherwise evaluates expr2</td></tr> 
</table>

<h3><a href="nyquist2">Next: Programming in Nyquist</a></h3>')?>

<?php
  include "../include/footer.inc.php";
?>
