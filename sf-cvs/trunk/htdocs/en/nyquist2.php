<?php BoxTop("Nyquist (2)"); ?>

<p>
<?php print "<a href=nyquist.php?$langLinkStr>";
?>Part 1: Introduction to Nyquist and Lisp programming.</a><br>
<?php print "<a href=nyquist2.php?$langLinkStr>";
?>Part 2: Programming in Nyquist.</a><br>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?>Part 3: Creating Nyquist plug-ins.</a><br>
</p>

<p>
What makes Nyquist distinct from Lisp is that it is designed to work with
sound, and has lots of built-in primitives and functions that synthesize,
analyze, and manipulate sounds.  Within Audacity, this makes it
relatively easy to build complicated effects out of Nyquist's palette
of filters.
</p>

<p>
In Nyquist, a variable can hold a sound.  A sound in Nyquist is not
as simple as a list of samples; it is best to think of a sound as an
object.  There are a lot of functions provided that allow you to
stretch, distort, and combine sounds very efficiently.  If you need to access
the individual samples of a sound, you can rip it apart and do so, but
Nyquist is not very efficient at doing this.
</p>

<p>

</p>

<p>
One of the easiest ways to manipulate sound is with the <tt>sim</tt>
function (short for <i>simultaneous</i>.  You can use it to combine
two or more sounds.  Suppose you have a sound in a variable <tt>a</tt>
and another sound in <tt>b</tt>.  To mix them together into a new
sound, <tt>c</tt>:
</p>

<pre>
  (setf c (sim a b))
</pre>

<p>

</p>

<p>
<b>Some trivia about Nyquist</b>
<ul>
<li>Symbols (like variable names and function names) are not
    case sensitive.  Nyquist converts them to uppercase internally.
</ul>
</p>

<p>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?><b>Next page</b></a>
</p>

<?php BoxBottom(); ?>

