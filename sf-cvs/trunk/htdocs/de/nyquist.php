<?php BoxTop("Nyquist"); ?>

<p>
<b>Part 1: Einführung in Nyquist und Lisp Programmierung.</b><br>
<?php print "<a href=nyquist2.php?$langLinkStr>";
?>Part 2: Programmierung in Nyquist.</a><br>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?>Part 3: Nyquist Plugins erstellen.</a><br>
</p>

<p>
Seit der Version 1.1.1, unterstützt Audacity die Nyquist
Programmierung um eigene Plugins für Effekte zu erstellen.
Im Gegensatz zu VST und LADSPA Plugins können Nyquist Plugins in einem normalen
Text Editor geschrieben werden und müssen nicht kompiliert werden.
</p>

<p>
Nyquist wurde "erfunden" von 
<a href="http://www-2.cs.cmu.edu/~rbd/">Roger B. Dannenberg</a>
und sollte eine komplette Programmiersprache für Audio Synthese und Analyse, sowie die
Unterstützung für MIDI, Audio Aufnahme und Wiedergabe, Datei I/O, Objekt-orientierte Programmierung,
profiling, debugging, und mehr.  Audacity benutzt nur einen kleinen Teil der Nyquist's Programmierung 
, die aber ausreichen um einfache Nyquist Funktionen zu benutzen und Audiodaten zu verändern.
Audacity unterstützt jedoch nicht das debugging des Nyquist Codes, es ist somit besser, den kompletten
Nyquist Code zu benutzen, wenn man komplizierte Audacity Plugins schreiben will.<br>
Nyquist bekommst du von dem Carnegie Mellon University Computer Music Project:
<ul>
<a href="http://www-2.cs.cmu.edu/~music/music.software.html"
>CMU Computer Music Project Software</a> - 
download den gesamten Nyquist Code hier</a>
<br>
<a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html"
>Das Handbuch findest du hier bei der CMU</a>
</ul>
</p>

<p>
Man benötigt jedoch nicht den kompletten Nyquist um Plugins für Audacity zu schreiben. 
Wie dies geht, wird nun beschrieben:
</p>
<p>
<b>(Anm. des Übersetzers (Pennywize): Ab hier wird es sehr technisch und man benötigt mathematische und technische Kenntnisse um dies zu verstehen.
Ebenso sollte man einigermassen Englisch sprechen um das ganze auch programmieren zu können, deswegen erfolgt keine weitere Übersetzung ab dieser Stelle.
</b>



</p>
<p><b>Lisp</b></p>

<p>
Nyquist basiert auf <i>Lisp</i>. When du bereits Erfahrungen mit Lisp
hast, kann man dieses Kapitel überspringen oder direkt zur 
<?php print "<a href=nyquist2.php?$langLinkStr>";
?><b>nächsten seite</b></a> gehen.
Für alle anderen, hier eine kurze Einführung in Lisp:
</p>

<p>
In Lisp (and therefore Nyquist), everything is an S-Expression,
which is just a list of tokens (words) separated by whitespace
and enclosed in parentheses.  The name of the function is always
the first token in an S-Expression, and all of the other tokens
are arguments to this function.  Here's a simple example:
</p>

<pre>
  (setf area (* 3.14159 (expt radius 2)))
</pre>

<p>
Let's break down this example.  The outermost S-expression has
three members.  The first one, <tt>setf</tt>, is the name of
the function (it stands for set-field).  <tt>setf</tt> is used to
assign a value to a variable.  (There are other similar functions,
like <tt>set</tt> and <tt>setq</tt>, but <tt>setf</tt> is the
most powerful, so it's the one we'll use in our examples.)
After <tt>setf</tt> comes <tt>area</tt>, which is the name of
the variable we're going to set.  Next comes the value to assign
to this variable, which in this case is another S-expression.
</p>

<p>
Lisp doesn't have any special operators for Math functions - they're
all functions like everything else, using <i>prefix</i> notation,
where the name of the function (or operator) come before its
arguments.  So instead of 3*7 for the product of 3 and 7, in Lisp
you would write (* 3 7).  In Nyquist, the <tt>expt</tt> (exponent)
function raises its first argument to the power of the second argument.
Therefore <tt>(* 3.14159 (expt radius 2))</tt> means 3.14159 times
the square of <tt>radius</tt>, or the formula for the area of a circle.
</p>

<p>
Rather than typing in this full expression every time, let's define
a function for the area of the circle, that we can call every time
we need it:
</p>

<pre>
  (defun circlearea (radius) (* 3.14159 (expt radius 2)))
</pre>

<p>
The <tt>defun</tt> function is used to define a new function.  The
first argument is the name of the function, in this case
<tt>circlearea</tt>.
The second argument is a list of arguments to the function to be
defined - this is one of the few cases where you have an S-expression
that is not interpreted as a function call.  Finally the last expression
is the value of the function.  Now if we want to compute the area
of a circle of radius <tt>r</tt>, we just need to compute:
</p>

<pre>
  (setf area (circlearea r))
</pre>

<p>
An S-expression is just a representation of a list.  Lisp uses
lists to represent just about everything (the name LISP comes
from LISt Processing language), so it's helpful to know
how to manipulate lists.  Let's start by assigning a list of
numbers to a variable.  You can't quite do this:
</p>

<pre>
  (setf mylist (1 2 3 4 5))  <font color=#cc0000><--  error!</font>
</pre>

<p>
The reason this doesn't work is that whenever Nyquist sees an
S-expression, it tries to evaluate it as a function unless you
tell it otherwise.  Since there's no function named "1" that
takes arguments <tt>(2 3 4 5)</tt>, this will generate an error.
To tell lisp that you want to treat an S-expression literally,
and not to evaluate it as a function, you <i>quote</i> it.
In Nyquist, you can quote a list by putting a single quotation
mark before it, like this:
</p>

<pre>
  (setf mylist '(1 2 3 4 5))
</pre>

<p>
Nyquist also provides a <tt>list</tt> function that you can use
to construct lists - this is useful if some of the elements of
the list are functions:
</p>

<pre>
  (setf mylist (list 1 2 3 4 (sqrt 25)))
</pre>

<p>
To get things off of a list, you can use the <tt>first</tt> and
<tt>rest</tt> functions.  (Traditionally, these were called
<tt>car</tt> and <tt>cdr</tt>, respectively, but <tt>first</tt> and
<tt>rest</tt> are much easier to remember.  Both sets of names are
supported in Nyquist.)  The output of <tt>(first mylist)</tt> is 1,
and the output of <tt>(rest mylist)</tt> is the list <tt>(2 3 4 5)</tt>.
So the second element of the list is <tt>(first (rest mylist))</tt>.
</p>

<p>
<b>Lisp function reference</b>
</p>

<p>
Here's a list of some of the basic lisp functions you might need.
</p>

<table border=0>
<tr><td align=center><i>Math functions</i></td><td></td></tr>
<tr><td><tt>(+ a b)</tt></td><td>addition</td></tr>
<tr><td><tt>(- a b)</tt></td><td>subtraction</td></tr>
<tr><td><tt>(* a b)</tt></td><td>multiplication</td></tr>
<tr><td><tt>(/ a b)</tt></td><td>division</td></tr>
<tr><td><tt>(truncate a b)</tt></td><td>round down to integer (floor)</td></tr>
<tr><td><tt>(float a b)</tt></td><td>integer to floating-point</td></tr>
<tr><td><tt>(rem a b c ...)</tt></td><td>remainder</td></tr>
<tr><td><tt>(min a b c ...)</tt></td><td>minimum</td></tr>
<tr><td><tt>(max a b c ...)</tt></td><td>maximum </td></tr>
<tr><td><tt>(abs a)</tt></td><td>absolute value</td></tr>
<tr><td><tt>(random n)</tt></td><td>random integer between 1 and n-1</td></tr>
<tr><td><tt>(sin a b)</tt></td><td>sine</td></tr>
<tr><td><tt>(cos a b)</tt></td><td>cosine</td></tr>
<tr><td><tt>(tan a b)</tt></td><td>tangent</td></tr>
<tr><td><tt>(expt a b)</tt></td><td>exponent (a to the power of b)</td></tr>
<tr><td><tt>(sqrt a b)</tt></td><td>square root</td></tr>
<tr><td><tt>(< a b)</tt></td><td>test for a less than b</td></tr>
<tr><td><tt>(<= a b)</tt></td><td>test for a less than or equal to b</td></tr>
<tr><td><tt>(> a b)</tt></td><td>test for a greater than b</td></tr>
<tr><td><tt>(>= a b)</tt></td><td>test for a greater than or equal to b</td></tr>
<tr><td><tt>(= a b)</tt></td><td>test for equality</td></tr>
<tr><td><tt>(/= a b)</tt></td><td>test for inequality</td></tr>
<tr><td align=center><i>List functions</i></td><td></td></tr>
<tr><td><tt>(first l)</tt></td><td>first element of a list (car)</td></tr>
<tr><td><tt>(rest l)</tt></td><td>rest of the list (cdr)</td></tr>
<tr><td><tt>(reverse l)</tt></td><td>reverse a list</td></tr>
<tr><td><tt>(list a b ...)</tt></td><td>construct a list</td></tr>
<tr><td><tt>(append l1 l2)</tt></td><td>append two lists</td></tr>
<tr><td><tt>(length l)</tt></td><td>length of a list</td></tr>
<tr><td><tt>(maplist function l)</tt></td><td>apply a function to every element in a list</td></tr>
<tr><td align=center><i>Control</i></td><td></td></tr>
<tr><td><tt>(if expr expr1 expr2)</tt></td><td>if expr is true, evaluates expr1, otherwise evaluates expr2</td></tr> 
</table>

<p>
For a complete list of Lisp / Nyquist functions, see the
<a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist Reference Manual</a>.
</p>

<p>
<b>Note: Symbols in Nyquist(like variable names and function names) are not
case sensitive.  They are converted to uppercase internally.</b>
</p>

<p>
<?php print "<a href=nyquist2.php?$langLinkStr>";
?><b>Next page: Programming in Nyquist</b></a>
</p>

<?php BoxBottom(); ?>
