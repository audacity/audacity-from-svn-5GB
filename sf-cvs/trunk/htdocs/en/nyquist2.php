<?php BoxTop("Nyquist (2)"); ?>

<p>
<?php print "<a href=nyquist.php?$langLinkStr>";
?>Part 1: Introduction to Nyquist and Lisp programming.</a><br>
<b>Part 2: Programming in Nyquist.</b><br>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?>Part 3: Creating Nyquist plug-ins.</a><br>
</p>

<p>
What makes Nyquist distinct from Lisp is that it is designed to work with
sound, and has lots of built-in primitives and functions that synthesize,
analyze, and manipulate sounds.  Within Audacity, this makes it
relatively easy to build complicated effects out of Nyquist's palette
of built-in functions.
</p>

<p>
In Nyquist, a variable can hold a sound just as easily as it
can hold a number or a string.  There are a lot of functions
provided that allow you to stretch, distort, and combine sounds
very efficiently.  It is even possible to "rip apart" a sound
and access its individual samples, but that's beyond the scope
of this tutorial.
</p>

<p>
<b>Synthesizing</b>
</p>

<p>
The following functions all create new sounds.  You can use
them to create "generate" plug-in effects, or you can combine
these synthesized sounds with selected audio to produce
interesting effects.
</p>

<table border=0>
<tr>
<td>(noise)</td>
<td>Generates white noise</td>
</tr>

<tr>
<td>(const value [duration])</td>
<td>Generates a constant (silent) signal</td>
</tr>

<tr>
<td>(sine pitch [duration])</td>
<td>Generates a sine wave at an indicated pitch and duration.<br>
    The pitch is a MIDI note number, with 60 for middle C.</td>
</tr>

<tr>
<td>(hzosc hz)</td>
<td>Generates a sine wave at a particular frequency in Hz.</td>
</tr>

<tr>
<td>(osc-saw hz)</td>
<td>Generates a sawtooth wave at a particular frequency in Hz.</td>
</tr>

<tr>
<td>(osc-tri hz)</td>
<td>Generates a triangle wave at a particular frequency in Hz.</td>
</tr>

<tr>
<td>(osc-pulse hz bias)</td>
<td>Generates a square pulse with variable width at the indicated
    frequency (in Hertz). The bias parameter controls the pulse width 
    and should be between -1 and +1, giving a pulse width from 0%
    (always at -1) to 100% (always at +1). When bias is zero,
    a square wave is generated.</td>
</tr>

<tr>
<td>(pluck pitch)</td>
<td>Generates a sound at the given pitch created using a
    modified Karplus-Strong plucked string algorithm.</td>
</tr>

</table>

<p>
<b>Envelopes</b>
</p>

<p>
Nyquist has support for envelopes.  By applying an envelope to
a sound, you can control the overall shape of its amplitude.
One of the easiest ways to construct an envelope is with the
<tt>env</tt> function, which takes 7 parameters that are commonly
used for shaping synthesized musical notes: attack time, decay
time, release time, attack level, decay level, sustain level, and
overall duration.  See the figure below:
</p>

<p>
<center>
<img src="/images/nyquist-envelope.gif" width=467 height=200>
</center>
</p>

<p>
To apply an envelope to a sound, just use the <tt>mult</tt> function.
So if <tt>s</tt> is a sound, then this is the sound with a simple
envelope applied to it:
</p>

<pre>
  (mult s (env 0.1 0.1 0.2 1.0 0.5 0.3 1.0))
</pre>

<p>
One of the most general type of envelope is a piece-wise linear
function, which can be constructed with the <tt>pwl</tt> function.
The pwl function takes a list of parameters which denote (time, value)
pairs. There is an implicit initial (time, value) pair of (0, 0),
and an implicit final value of 0. There should always be an odd number
of parameters, since the final time is not implicit.  For example:
</p>

<pre>
  ; symmetric rise to 0.7 (at time 1) and fall back to 0 (at time 2):
  (pwl 1.0 0.7 2.0)
</pre>

<p>
<b>Combining sounds</b>
</p>

<p>
Besides multiplying two sounds with the <tt>mult</tt> function,
you can add two sounds (or envelopes) with the <tt>add</tt> function.
</p>

<p>
<b>Filters</b>
</p>

<p>
Nyquist comes with a number of common filters built-in.  Here are
some of the more common ones:
</p>

<table border=0>

<tr>
<td>(lp sound cutoff)</td>
<td>Low-pass filter (first-order Butterworth). Cutoff may be a float or a signal (for time-varying filtering) and expresses hertz.</td>
</tr>

<tr>
<td>(hp sound cutoff)</td>
<td>High-pass filter (first-order Butterworth). Cutoff may be a float or a signal (for time-varying filtering) and expresses hertz.</td>
</tr>

<tr>
<td>(comb sound hz decay)</td>
<td>Applies a comb filter to sound, which emphasizes
    (resonates at) frequencies that are multiples of a hz.
</td>
</tr>

<tr>
<td>(alpass sound decay hz)</td>
<td>All-pass filter, creating a delay effect without the resonances
    of a comb filter.  </td>
</tr>

<tr>
<td>(notch2 sound hz)</td>
<td>Second-order notch filter centered at a frequency of hz.</td>
</tr>

</table>

<p>
<b>Transforming and combining sounds</b>
</p>

<p>
It is beyond the scope of this introductory tutorial to explain
all of the ways that a sound can be transformed in Nyquist.
These functions do not modify sounds directly, but instead modify
the Nyquist <i>environment</i>.  In order for these changes to
affect sounds, you must use the <tt>cue</tt> function.
</p>

<table border=0>

<tr>
<td>(stretch factor (cue sound))</td>
<td>Changes the length of the sound being cued by the given factor.</td>
</tr>

<tr>
<td>(scale factor (cue sound))</td>
<td>Scales the amplitude of the sound being cued by the given factor.</td>
</tr>

<tr>
<td>(loud dB (cue sound))</td>
<td>Increases or decreases the volume of the sound being cued by the<br>
    given number of decibels.</td>
</tr>

<tr>
<td>(at t (cue sound))</td>
<td>Starts the given sound at a particular time in seconds.<br>
    This can't be used to add silence at the beginning or end,<br>
    but it can be used when combining two or more sounds.</td>
</tr>

<tr>
<td>(seq (cue s1) (cue s2))</td>
<td>Creates a sequence of sound s1 followed by sound s2.</td>
</tr>

<tr>
<td>(sim (cue s1) (cue s2))</td>
<td>Combines two sounds so that they are played simultaneously.
</tr>

</table>

<p>
For more information, please read the 
<a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist Reference Manual</a>.
</p>

<p>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?><b>Next page: Creating Nyquist plug-ins</b></a>
</p>

<?php BoxBottom(); ?>

