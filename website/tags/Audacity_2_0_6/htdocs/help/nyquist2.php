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

<?php
printf('<p>%s</p>', _('What makes Nyquist distinct from Lisp is that it is designed to work with sound, and has lots of built-in primitives and functions that synthesize, analyze, and manipulate sounds.  Within Audacity, this makes it relatively easy to build complicated effects out of Nyquist\'s palette of built-in functions.'));
printf('<p>%s</p>', _('In Nyquist, a variable can hold a sound just as easily as it can hold a number or a string.  There are a lot of functions provided that allow you to stretch, distort, and combine sounds very efficiently.  It is even possible to "rip apart" a sound and access its individual samples, but that\'s beyond the scope of this tutorial.'));
printf(_('%sTo try out a Nyquist expression in Audacity, you can use "Nyquist Prompt" in the Effect menu.  Whatever audio you have selected will be in the variable %s, and the selection will be replaced with the result of the Nyquist expression you enter.  In Part 3, you will learn how to create a plug-in effect using Nyquist.%s'), '<p>','<code>s</code>', '</p>');

printf('<h3>%s</h3>', _('Synthesizing'));

printf('<p>%s</p>', _('The following functions all create new sounds.  You can use them to create "generate" plug-in effects, or you can combine these synthesized sounds with selected audio to produce interesting effects.'));
?>
<table class="function-list" >
<?php
printf('<tr><td>(noise)</td><td>%s</td></tr>', _('Generates white noise'));
printf('<tr><td>(const value [duration])</td><td>%s</td></tr>', _('Generates a constant (silent) signal'));
printf('<tr><td>(sine pitch [duration])</td><td>%s<br>%s</td></tr>', _('Generates a sine wave at an indicated pitch and duration.'), _('The pitch is a MIDI note number, with 60 for middle C.'));
printf('<tr><td>(hzosc hz)</td><td>%s</td></tr>', _('Generates a sine wave at a particular frequency in Hz.'));
printf('<tr><td>(osc-saw hz)</td><td>%s</td></tr>', _('Generates a sawtooth wave at a particular frequency in Hz.'));
printf('<tr><td>(osc-tri hz)</td><td>%s</td></tr>', _('Generates a triangle wave at a particular frequency in Hz.'));
printf('<tr><td>(osc-pulse hz bias)</td><td></td></tr>', _('Generates a square pulse with variable width at the indicated frequency (in Hertz). The bias parameter controls the pulse width and should be between -1 and +1, giving a pulse width from 0% (always at -1) to 100% (always at +1). When bias is zero, a square wave is generated.'));
printf('<tr><td>(pluck pitch)</td><td></td></tr>', _('Generates a sound at the given pitch created using a modified Karplus-Strong plucked string algorithm.'));
?>
</table><?php
printf('<p><b>%s</b></p>', _('Envelopes'));
printf(_('%sNyquist has support for envelopes.  By applying an envelope to a sound, you can control the overall shape of its amplitude.  One of the easiest ways to construct an envelope is with the %s function, which takes 7 parameters that are commonly used for shaping synthesized musical notes: attack time, decay time, release time, attack level, decay level, sustain level, and overall duration.'), '<p>', '<code>env</code>');
printf(_('See the figure below:'));
?>
</p>
<?php
printf('<p><img alt="%s" src="../images/nyquist-envelope" width="467" height="200"></p>', _('Nyquist envelope diagram.'));
printf(_('%sTo apply an envelope to a sound, just use the %s function.  So if %s is a sound, then this is the sound with a simple envelope applied to it:%s'), '<p>', '<code>mult</code>', '<code>s</code>', '</p>');
?>
<pre>
  (mult s (env 0.1 0.1 0.2 1.0 0.5 0.3 1.0))
</pre>
<?php
printf(_('%sOne of the most general type of envelope is a piece-wise linear function, which can be constructed with the %s function.  The pwl function takes a list of parameters which denote (time, value) pairs. There is an implicit initial (time, value) pair of (0, 0), and an implicit final value of 0. There should always be an odd number of parameters, since the final time is not implicit.  For example:'), '<p>', '<code>pwl</code>', '</p>');
?>
<pre>
  ; symmetric rise to 0.7 (at time 1) and fall back to 0 (at time 2):
  (pwl 1.0 0.7 2.0)
</pre>
<?php
printf('<h3>%s</h3>', _('Combining sounds'));

printf(_('%sBesides multiplying two sounds with the %s function, you can add two sounds (or envelopes) with the %s function.%s'), '<p>', '<code>mult</code>', '<code>add</code>', '</p>');

printf('<h3>%s</h3>', _('Filters'));

printf('<p>%s</p>', _('Nyquist comes with a number of common filters built-in.  Here are some of the more common ones:'));
?>
<table class="function-list">
<?php
printf('<tr><td>(lp sound cutoff)</td><td></td></tr>', 
// i18n-hint: Butterworth is the name of the filter design. Not normally something you'd want to translate.
_('Low-pass filter (first-order Butterworth). Cutoff may be a float or a signal (for time-varying filtering) and expresses hertz.'));
printf('<tr><td>(hp sound cutoff)</td><td>%s</td></tr>', _('High-pass filter (first-order Butterworth). Cutoff may be a float or a signal (for time-varying filtering) and expresses hertz.'));
printf('<tr><td>(comb sound hz decay)</td><td>%s</td></tr>', _('Applies a comb filter to sound, which emphasizes (resonates at) frequencies that are multiples of a Hz.'));
printf('<tr><td>(alpass sound decay hz)</td><td>%s</td></tr>', _('All-pass filter, creating a delay effect without the resonances of a comb filter.'));
printf('<tr><td>(notch2 sound hz)</td><td></td></tr>', _('Second-order notch filter centered at a frequency of hz.'));
?>
</table>
<?php
printf('<h3>%s</h3>', _('Transforming and combining sounds'));

printf(_('%sIt is beyond the scope of this introductory tutorial to explain all of the ways that a sound can be transformed in Nyquist.  These functions do not modify sounds directly, but instead modify the Nyquist %senvironment%s.  In order for these changes to affect sounds, you must use the %s function.%s'), '<p>', '<em>', '</em>', '<code>cue</code>', '</p>');
?>
<table class="function-list">
<?php
printf('<tr><td>(stretch factor (cue sound))</td><td>%s</td></tr>', _('Changes the length of the sound being cued by the given factor.'));
printf('<tr><td>(scale factor (cue sound))</td><td>%s</td></tr>', _('Scales the amplitude of the sound being cued by the given factor.'));
printf('<tr><td>(loud dB (cue sound))</td><td>%s</td></tr>', _('Increases or decreases the volume of the sound being cued by the given number of decibels.'));
printf('<tr><td>(at t (cue sound))</td><td>%s</td></tr>', _('Starts the given sound at a particular time in seconds. This can\'t be used to add silence at the beginning or end, but it can be used when combining two or more sounds.'));
printf('<tr><td>(seq (cue s1) (cue s2))</td><td>%s</td></tr>', _('Creates a sequence of sound s1 followed by sound s2.'));
printf('<tr><td>(sim (cue s1) (cue s2))</td><td>%s</td></tr>', _('Combines two sounds so that they are played simultaneously.'));
?>
</table>


<?php
printf('<h3><a href="nyquist3">%s</a></h3>', _('Next: Creating Nyquist Plug-Ins'));
  include "../include/footer.inc.php";
?>
