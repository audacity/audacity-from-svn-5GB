<?php BoxTop("Nyquist (2)"); ?>

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
<b>Some trivia about Nyquist</b>
<ul>
<li>Symbols (like variable names and function names) are not
    case sensitive.  Nyquist converts them to uppercase internally.
</ul>
</p>

<?php BoxBottom(); ?>

