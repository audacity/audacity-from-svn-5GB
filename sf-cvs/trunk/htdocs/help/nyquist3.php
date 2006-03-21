<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2003 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "nyquist";
  $pageTitle = _("Creating Nyquist Plug-ins");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<ol>
	<li><a href="nyquist"><?=_("Introduction to Nyquist and Lisp Programming")?></a></li>
	<li><a href="nyquist2"><?=_("Programming in Nyquist")?></a></li>
	<li><b><?=_("Creating Nyquist Plug-ins")?></b></li>
</ol>

<?=_('<p>Creating a plug-in for Audacity using Nyquist is as simple as creating a text file with the extension ".ny" with some Nyquist code, adding a few comments to indicate the type of plug-in, and placing the file in Audacity\'s <code>plug-ins</code> directory.  Here\'s a very simple plug-in as an example:</p>

<pre>
  ;nyquist plug-in
  ;version 1
  ;type process
  ;name "Fade In"
  ;action "Fading In..."
  (mult (ramp) s)
</pre>

<p>The first two lines of a Nyquist plug-in must be exactly as in the example above, or Audacity will not load it.  (The version number is to support future expansions of the plug-in format.) The next line is the type of plug-in, which is discussed below.  Then comes the name of the plug-in, which is what is displayed in the menu bar, and then the "action", which is what Audacity displays while it is busy processing the plug-in.  There are other optional lines that may follow.  Any line that does not begin with a semicolon (;) is assumed to contain Nyquist code and will be executed.</p>

<p>Audacity has support for three types of plug-ins that can be written in Nyquist:</p>

<pre>
  ;type generate
  ;type process
  ;type analyze
</pre>

<p>These correspond to the three menus that can contain plug-ins: Generate, Effect, and Analyze.  Generate plug-ins are expected to generate new audio from scratch, Effect plug-ins ("process") modify existing audio in-place, and Analyze plug-ins process audio but do not modify it (though they are allowed to add labels).</p>

<p>For Effect and Analyze plug-ins, Audacity sets up the Nyquist environment so that the audio the user has selected is in the variable <code>s</code>.  All of the expressions in the plug-in file are executed in order, and the return value of the last expression is substituted for the selection in Audacity.  If the last expression does not return audio, Audacity returns an error.</p>

<h3>Parameter dialogs</h3>

<p>Audacity has limited support for plug-ins showing a dialog to get parameters from the user.  Here is an example of a plug-in that opens a dialog:</p>

<pre>
  ;nyquist plug-in
  ;version 1
  ;type process
  ;name "Delay..."
  ;action "Performing Delay Effect..."
  ;info "Demo effect for Nyquist by Roger Dannenberg.\nThis effect 
     creates a fixed number of echos."  ; (should be all on one line)
  ;control decay "Decay amount" int "dB" 6 0 24
  ;control delay "Delay time" real "seconds" 0.5 0.0 5.0
  ;control count "Number of echos" int "times" 5 1 30

  (defun delays (s decay delay count)
    (if (= count 0) (cue s)
	   (sim (cue s)
        (loud decay (at delay (delays s decay delay (- count 1)))))))
  (stretch-abs 1 (delays s (- 0 decay) delay count))
</pre>

<p>If Audacity finds at least one "control" line, it will open a dialog to prompt the user for certain parameters to the plug-in.  Each parameter consists of a text box and a slider, and after the user has entered each one, the final value will be stored in a Nyquist variable with a name that you specify on the "control" line.  Here\'s what the dialog for the Delay effect shown above looks like in Audacity for Linux:</p>

<p><img alt="Nyquist plugin screenshot" src="../images/nyquist-plugin-screenshot" width="414" height="221"></p>

<p>Note that the "info" line is displayed at the top of the dialog, and that the "\n" becomes a newline.  The parameters to the "control" line affect the appearance and limitations of the parameter.  Each "control" line must consist of exactly the following 8 elements, in order:</p>

<ol>
	<li>The word "control"</li>
	<li>The name of the control - this is the name of the Nyquist variable that will get set when the user manipulates the dialog.</li>
	<li>The label to the left of the control</li>
	<li>The type of value: either <code>int</code> (integer) or <code>real</code>.</li>
	<li>The label to the right of the value (usually the units like "Hz" or "dB").</li>
	<li>The default/initial value of the parameter</li>
	<li>The minimum value of the parameter</li>
	<li>The maximum value of the parameter</li>
</ol>

<h3>Returning labels</h3>

<p>Instead of returning audio, a Nyquist plug-in can instead return a list of labels.  A list of labels is simply a list of time/label pairs, for example:</p>

<pre>
  ((0.0 "start") (30.0 "middle") (60.0 "end"))
</pre>

<p>When a plug-in returns a list of exactly this form, Audacity will create a new label track and add the labels at those positions.  This style of plug-in is usually of type "analyze".</p>

<p>New!  Beginning with Audacity version 1.3.1, you can now optionally return both a start and an end time, like this:</p>

<pre>
  ((0.0 25.0 "start") (30.0 45.0 "middle") (60.0 75.0 "end"))
</pre>

<p>Note that labels are allowed to overlap in Audacity 1.3; the end time of one can be after the start time of the next.</p>

<h3>Processing stereo tracks</h3>

<p>Nyquist represents stereo tracks as an array of sounds (not a list).  Many Nyquist functions automatically work with these arrays, but not all, so sometimes you may find it necessary to split up a stereo array, or reassemble one.  Here are some useful functions:</p>

<table class="function-list">
<tr><td><code>(arrayp s)</code></td><td>returns true if <code>s</code> is an array</td></tr>
<tr><td><code>(aref s 0)</code></td><td>the first element in array <code>s</code> - the left channel</td></tr>
<tr><td><code>(aref s 1)</code></td><td>the second element in array <code>s</code> - the right channel</td></tr>
<tr><td><code>(setf s (make-array 2))</code></td><td>makes <code>s</code> into a new array of length 2</td></tr>
<tr><td><code>(setf (aref s 0) left)</code></td><td>makes <code>left</code> the first element of array <code>s</code></td></tr>
<tr><td><code>(setf (aref s 1) right)</code></td><td>makes <code>right</code> the second element of array <code>s</code></td></tr>
</table>

<p>As a convenience, if the input to your Nyquist plug-in is stereo, but you only output a single (mono) sound, Audacity will automatically copy it to both the left and right channels.</p>

<h3>Where to go from here</h3>

<p>Audacity comes with some sample plug-ins that you can examine or modify as a starting point.  The best way to learn Nyquist is to try it.  If you\'re having trouble debugging, consider downloading the standalone version of Nyquist (see the link in Part 1).</p>

<p>If you\'re having trouble getting Nyquist to work at all, please <a href="../contact/">contact us</a>.</p>

<p>If you are working on Nyquist plug-in effects and would like to share them with others, or if you would like to discuss general details of how to achieve certain effects in Nyquist, please join the <a href="../contact/lists#nyquist">audacity-nyquist mailing list</a>.</p>

<p>Don\'t forget to consult the full <a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist Reference Manual</a> for more details of how Nyquist works.</p>')?>

<?php
  include "../include/footer.inc.php";
?>
