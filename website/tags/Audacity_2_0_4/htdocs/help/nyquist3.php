<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2003 - 11 Dominic Mazzoni, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
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

<?php
printf(_('%sCreating a plug-in for Audacity using Nyquist is as simple as creating a text file with the extension ".ny" with some Nyquist code, adding a few comments to indicate the type of plug-in, and placing the file in Audacity\'s %s directory.  Here\'s a very simple plug-in as an example:%s'), '<p>', '<code>plug-ins</code>', '</p>');
?>
<pre>
  ;nyquist plug-in
  ;version 1
  ;type process
  ;name "Fade In"
  ;action "Fading In..."
  (mult (ramp) s)
</pre>
<?php
printf('<p>%s</p>', _('The first line of a Nyquist plug-in must be exactly as in the example above, and the second line must indicate a version number. Version 2 and 3 plug-ins support more widgets, but version 3 plug-ins are not supported in Audacity 1.3.3 or earlier. The next line is the type of plug-in, which is discussed below.  Then comes the name of the plug-in, which is what is displayed in the menu bar, and then the "action", which is what Audacity displays while it is busy processing the plug-in.  There are other optional lines that may follow.  Any line that does not begin with a semicolon (;) is assumed to contain Nyquist code and will be executed.'));
printf('<p>%s</p>', _('Audacity has support for three types of plug-ins that can be written in Nyquist:'));
?>
<pre>
  ;type generate
  ;type process
  ;type analyze
</pre>
<?php 
printf('<p>%s</p>', _('These correspond to the three menus that can contain plug-ins: Generate, Effect, and Analyze.  Generate plug-ins are expected to generate new audio from scratch, Effect plug-ins ("process") modify existing audio in-place, and Analyze plug-ins process audio but do not modify it (though they are allowed to add labels).'));

printf(_('%sFor Effect and Analyze plug-ins, Audacity sets up the Nyquist environment so that the audio the user has selected is in the variable %s.  All of the expressions in the plug-in file are executed in order, and the return value of the last expression is substituted for the selection in Audacity.  If the last expression does not return audio, Audacity returns an error.%s'), '<p>', '<code>s</code>', '</p>');

printf('<h3>%s</h3>', _('Parameter dialogs'));

printf('<p>%s</p>', _('Audacity has limited support for plug-ins showing a dialog to get parameters from the user.  Here is an example of a plug-in that opens a dialog:'));
?>
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
<?php
printf('<p>%s</p>', _('If Audacity finds at least one "control" line, it will open a dialog to prompt the user for certain parameters to the plug-in.  Each parameter consists of a text box and a slider, and after the user has entered each one, the final value will be stored in a Nyquist variable with a name that you specify on the "control" line.  Here\'s what the dialog for the Delay effect shown above looks like in Audacity for Linux:'));

printf('<p><img alt="%s" src="../images/nyquist-plugin-screenshot" width="414" height="221"></p>', _('Nyquist plugin screenshot'));

printf('<p>%s</p>', _('Note that the "info" line is displayed at the top of the dialog, and that the "\n" becomes a newline.  The parameters to the "control" line affect the appearance and limitations of the parameter.  Each "control" line must consist of exactly the following 8 elements, in order:'));

printf('<ol><li>%s</li>', _('The word "control"'));
printf('<li>%s</li>', _('The name of the control - this is the name of the Nyquist variable that will get set when the user manipulates the dialog.'));
printf('<li>%s</li>', _('The label to the left of the control'));
printf(_('%sThe type of value: either %s (integer) or %s.'), '<li>', '<code>int</code>', '<code>real</code>', '</li>');
printf('<li>%s</li>', _('The label to the right of the value (usually the units like "Hz" or "dB").'));
printf('<li>%s</li>', _('The default/initial value of the parameter'));
printf('<li>%s</li>', _('The minimum value of the parameter'));
printf('<li>%s</li></ol>', _('The maximum value of the parameter'));

printf('<h3>%s</h3>', _('Returning labels'));

printf('<p>%s</p>', _('Instead of returning audio, a Nyquist plug-in can instead return a list of labels.  A list of labels is simply a list of time/label pairs, for example:'));
?>
<pre>
  ((0.0 "start") (30.0 "middle") (60.0 "end"))
</pre>
<?php
printf('<p>%s</p>', _('When a plug-in returns a list of exactly this form, Audacity will create a new label track and add the labels at those positions.  This style of plug-in is usually of type "analyze".'));

printf('<p>%s</p>', _('New!  Beginning with Audacity version 1.3.1, you can now optionally return both a start and an end time, like this:'));
?>
<pre>
  ((0.0 25.0 "start") (30.0 45.0 "middle") (60.0 75.0 "end"))
</pre>
<?php
printf('<p>%s</p>', _('Note that labels are allowed to overlap in Audacity 1.3; the end time of one can be after the start time of the next.'));

printf('<h3>%s</h3>', _('Processing stereo tracks'));

printf('<p>%s</p>', _('Nyquist represents stereo tracks as an array of sounds (not a list).  Many Nyquist functions automatically work with these arrays, but not all, so sometimes you may find it necessary to split up a stereo array, or reassemble one.  Here are some useful functions:'));
?>
<table class="function-list">
<?php
// i18n-hint: The first and last are html tags, the middle one is a variable name
printf(_('%sreturns true if %s is an array%s'), '<tr><td><code>(arrayp s)</code></td><td>', '<code>s</code>', '</td></tr>');
printf(_('%sthe first element in array %s - the left channel%s'), '<tr><td><code>(aref s 0)</code></td><td>', '<code>s</code>', '</td></tr>');
printf(_('%sthe second element in array %s - the right channel%s'), '<tr><td><code>(aref s 1)</code></td><td>', '<code>s</code>', '</td></tr>');
printf(_('%smakes %s into a new array of length 2%s'), '<tr><td><code>(setf s (make-array 2))</code></td><td>', '<code>s</code>', '</td></tr>');
printf(_('%smakes %s the first element of array %s'), '<tr><td><code>(setf (aref s 0) left)</code></td><td>', '<code>left</code>', '<code>s</code></td></tr>');
printf(_('%smakes %s the second element of array %s'), '<tr><td><code>(setf (aref s 1) right)</code></td><td>', '<code>right</code>', '<code>s</code></td></tr>'); ?>
</table>
<?php
printf('<p>%s</p>', _('As a convenience, if the input to your Nyquist plug-in is stereo, but you only output a single (mono) sound, Audacity will automatically copy it to both the left and right channels.'));

printf('<h3>%s</h3>', _('Where to go from here'));

printf('<p>%s</p>', _('Audacity comes with some sample plug-ins that you can examine or modify as a starting point.  The best way to learn Nyquist is to try it.  If you\'re having trouble debugging, consider downloading the standalone version of Nyquist (see the link in Part 1).'));
//i18n-hint: All the %s are HTML tags which makes "contact us" into a link
printf(_('%sIf you\'re having trouble getting Nyquist to work at all, please %scontact us%s.%s'), '<p>', '<a href="../contact/#feedback">', '</a>', '</p>');
//i18n-hint: All the %s are HTML tags which makes the text into links.
printf(_('%sIf you are working on Nyquist plug-in effects and would like to share them with others, or if you would like to discuss how to achieve certain effects in Nyquist, please post on the %sNyquist board of the Audacity Forum%s or join the %saudacity-nyquist mailing list%s.%s'), '<p>', '<a href="http://forum.audacityteam.org/viewforum.php?f=39">', '</a>', '<a href="../contact/lists#nyquist">', '</a>', '</p>');
//i18n-hint: All the %s are HTML tags which makes "Nyquist Reference Manual" into a link
printf(_('%sDon\'t forget to consult the full %sNyquist 2.37 Reference Manual%s for more details of Lisp and Nyquist.%s'), '<p>', '<a href="http://www.audacity-forum.de/download/edgar/nyquist/nyquist-doc/manual/home.html">', '</a>', '</p>');


  include "../include/footer.inc.php";
?>
