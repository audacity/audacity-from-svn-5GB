<?php BoxTop("Nyquist (3)"); ?>

<p>
<b>Creating Nyquist plug-in effects for Audacity</b>
</p>

<p>
Creating a plug-in for Audacity using Nyquist is as simple as
creating a text file with the extension ".ny" with some Nyquist
code, adding a few comments to indicate the type of plug-in, and
placing the file in Audacity's <tt>plug-ins</tt> directory.
Here's a very simple plug-in as an example:
</p>

<xmp>
  ;nyquist plug-in
  ;version 1
  ;type process
  ;name "Fade In"
  ;action "Fading In..."
  (mult (ramp) s)
</xmp>

<p>
The first two lines of a Nyquist plug-in must be exactly as in
the example above, or Audacity will not load it.  (The version
number is to support future expansions of the plug-in format.)
The next line is the type of plug-in, which is discussed below.
Then comes the name of the plug-in, which is what is displayed
in the menu bar, and then the "action", which is what Audacity
displays while it is busy processing the plug-in.
There are other optional lines that may follow.
Any line that does not
begin with a semicolon (;) is assumed to contain Nyquist code
and will be executed.
</p>

<p>
Audacity has support for three types of plug-ins that can be written
in Nyquist:

<pre>
  ;type generate
  ;type process
  ;type analyze
</pre>

These correspond to the three menus that can contain
plug-ins: Generate, Effect, and Analyze.  Generate plug-ins
are expected to generate new audio from scratch, Effect plug-ins
("process") modify existing audio in-place, and Analyze plug-ins process audio
but do not modify it (though they are allowed to add labels).
</p>

<p>
For Effect and Analyze plug-ins, Audacity sets up the Nyquist
environment so that the audio the user has selected is in the
variable <tt>s</tt>.  All of the expressions in the plug-in
file are executed in order, and the return value of the last
expression is substituted for the selection in Audacity.
If the last expression does not return audio, Audacity returns
an error.
</p>

<p>
<b>Parameter dialogs</b>
</p>

<p>
Audacity has limited support for plug-ins showing a dialog
to get parameters from the user.  Here is an example of a
plug-in that opens a dialog:
</p>

<xmp>
  ;nyquist plug-in
  ;version 1
  ;type process
  ;name "Delay..."
  ;action "Performing Delay Effect..."
  ;info "Demo effect for Nyquist by Roger Dannenberg.\nThis effect creates a fixed number of echos."
  ;control decay "Decay amount" int "dB" 6 0 24
  ;control delay "Delay time" real "seconds" 0.5 0.0 5.0
  ;control count "Number of echos" int "times" 5 1 30

  (defun delays (s decay delay count)
    (if (= count 0) (cue s)
	   (sim (cue s)
        (loud decay (at delay (delays s decay delay (- count 1)))))))
  (stretch-abs 1 (delays s (- 0 decay) delay count))
</xmp>

<p>
If Audacity finds at least one "control" line, it will open a dialog
to prompt the user for certain parameters to the plug-in.  Each
parameter consists of a text box and a slider, and after the user
has entered each one, the final value will be stored in a Nyquist
variable with a name that you specify on the "control" line.  Here's
what the dialog for the Delay effect shown above looks like
in Audacity for Linux:
</p>

<p>
<center>
<img src="/images/delay_effect_dialog.png" width=414 height=221>
</center>
</p>

<p>
Note that the "info" line is displayed at the top of the dialog,
and that the "\n" becomes a newline.  The parameters to the
"control" line affect the appearance and limitations of the
parameter.  Each "control" line must consist of exactly the
following 8 elements, in order:
</p>

<ol>
<li>The word "control"
<li>The name of the control - this is the name of the Nyquist variable
that will get set when the user manipulates the dialog.
<li>The label to the left of the control
<li>The type of value: either <tt>int</tt> (integer) or
    <tt>real</tt>.
<li>The label to the right of the value (usually the units like
    "Hz" or "dB").
<li>The default/initial value of the parameter
<li>The minimum value of the parameter
<li>The maximum value of the parameter
</ol>

<p>
<b>Returning labels
</p>

<p>
Instead of returning audio, a Nyquist plug-in can instead return
a list of labels.  A list of labels is simply a list of
time/label pairs, for example:
</p>

<pre>
  ((0.0 "start") (30.0 "middle") (60.0 "end"))
</pre>

<p>
When a plug-in returns a list of exactly this form, Audacity
will create a new label track and add the labels at those
positions.  This style of plug-in is usually of type "analyze".
</p>

<p>
<b>Processing stereo tracks</b>
</p>

<p>
Nyquist represents stereo tracks as an array of sounds
(not a list).  Many Nyquist functions automatically work
with these arrays, but not all, so sometimes you may find
it necessary to split up a stereo array, or reassemble one.
Here are some useful functions:
</p>

<table border=0>
<tr><td><tt>(arrayp s)</tt></td><td>returns true if <tt>s</tt> is an array</td></tr>
<tr><td><tt>(aref s 0)</tt></td><td>the first element in array <tt>s</tt> - the left channel</td></tr>
<tr><td><tt>(aref s 1)</tt></td><td>the second element in array <tt>s</tt> - the right channel</td></tr>
<tr><td><tt>(setf s (make-array 2))</tt></td><td>makes <tt>s</tt> into a new array of length 2</td></tr>
<tr><td><tt>(setf (aref s 0) left)</tt></td><td>makes <tt>left</tt> the first element of array <tt>s</tt></td></tr>
<tr><td><tt>(setf (aref s 1) right)</tt></td><td>makes <tt>right</tt> the second element of array <tt>s</tt></td></tr>
</table>

<?php BoxBottom(); ?>


