<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2003 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "nyquist";
  $pageTitle = _("Programming in Nyquist");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<ol>
	<li><a href="nyquist"><?=_("Introduction to Nyquist and Lisp Programming")?></a></li>
	<li><b><?=_("Programming in Nyquist")?></b></li>
	<li><a href="nyquist3"><?=_("Creating Nyquist Plug-ins")?></a></li>
</ol>

<?=_('<p>What makes Nyquist distinct from Lisp is that it is designed to work with sound, and has lots of built-in primitives and functions that synthesize, analyze, and manipulate sounds.  Within Audacity, this makes it relatively easy to build complicated effects out of Nyquist\'s palette of built-in functions.</p>

<p>In Nyquist, a variable can hold a sound just as easily as it can hold a number or a string.  There are a lot of functions provided that allow you to stretch, distort, and combine sounds very efficiently.  It is even possible to "rip apart" a sound and access its individual samples, but that\'s beyond the scope of this tutorial.</p>

<p>To try out a Nyquist expression in Audacity, you can use "Nyquist Prompt" in the Effect menu.  Whatever audio you have selected will be in the variable <code>s</code>, and the selection will be replaced with the result of the Nyquist expression you enter.  In Part 3, you will learn how to create a plug-in effect using Nyquist.</p>

<h3>Synthesizing</h3>

<p>The following functions all create new sounds.  You can use them to create "generate" plug-in effects, or you can combine these synthesized sounds with selected audio to produce interesting effects.</p>

<table class="function-list" class="function-list">
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

<p>Nyquist has support for envelopes.  By applying an envelope to a sound, you can control the overall shape of its amplitude.  One of the easiest ways to construct an envelope is with the <code>env</code> function, which takes 7 parameters that are commonly used for shaping synthesized musical notes: attack time, decay time, release time, attack level, decay level, sustain level, and overall duration.  See the figure below:</p>

<p><img alt="Nyquist envelope diagram." src="../images/nyquist-envelope" width="467" height="200"></p>

<p>To apply an envelope to a sound, just use the <code>mult</code> function.  So if <code>s</code> is a sound, then this is the sound with a simple envelope applied to it:</p>

<pre>
  (mult s (env 0.1 0.1 0.2 1.0 0.5 0.3 1.0))
</pre>

<p>One of the most general type of envelope is a piece-wise linear function, which can be constructed with the <code>pwl</code> function.  The pwl function takes a list of parameters which denote (time, value) pairs. There is an implicit initial (time, value) pair of (0, 0), and an implicit final value of 0. There should always be an odd number of parameters, since the final time is not implicit.  For example:</p>

<pre>
  ; symmetric rise to 0.7 (at time 1) and fall back to 0 (at time 2):
  (pwl 1.0 0.7 2.0)
</pre>

<h3>Combining sounds</h3>

<p>Besides multiplying two sounds with the <code>mult</code> function, you can add two sounds (or envelopes) with the <code>add</code> function.</p>

<h3>Filters</h3>

<p>Nyquist comes with a number of common filters built-in.  Here are some of the more common ones:</p>

<table class="function-list">

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

<h3>Transforming and combining sounds</h3>

<p>It is beyond the scope of this introductory tutorial to explain all of the ways that a sound can be transformed in Nyquist.  These functions do not modify sounds directly, but instead modify the Nyquist <em>environment</em>.  In order for these changes to affect sounds, you must use the <code>cue</code> function.</p>

<table class="function-list">

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
    This can\'t be used to add silence at the beginning or end,<br>
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

<h3><a href="nyquist3">Next: Creating Nyquist Plug-Ins</a></h3>')?>

<?php
  include "../include/footer.inc.php";
?>
