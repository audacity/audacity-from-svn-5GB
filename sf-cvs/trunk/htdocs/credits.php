<?php

require_once 'main.inc.php';
$title = $creditsStr;
include 'top.inc.php';

BoxTop($creditsStr);

?>

<p>
General comments and questions about Audacity should be sent to 
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>.
</p>

<h3>Lead Developers</h3>

<p>
<b>Dominic Mazzoni</b> is the project leader and 
Audacity's primary programmer.  He developed almost all of the
initial code for Audacity, and is currently working on improving
support for 32-bit samples and dithering, improving resampling,
and adding a new "Smart Record" feature which gets the most
accurate and reliable recording possible by minimizing CPU
and disk usage.
He began Audacity while a graduate student at 
Carnegie Mellon University in Pittsburgh, PA.
He is currently a research programmer at the
NASA Jet Propulsion Laboratory in Pasadena, CA.
<br>
<tt>email: dominic <b>@</b> minorninth.com</tt><br>
<a href="http://Dominic-Mazzoni.com">web page</a><br>
</p>

<p>
<b>Joshua Haberman</b> is a primary Audacity developer.
He wrote most of the Preferences dialog,
all Ogg Vorbis support, MP3 exporting support, plus lots of
other features and bug fixes.  He maintains the Audacity
for Debian GNU/Linux.
He is a student at the University of Puget Sound in Tacoma, WA,
and working for Myricom, Inc in Arcadia, CA for the summer of 2002.
<br>
<tt>email: joshua <b>@</b> haberman.com</tt><br>
<a href="http://www.reverberate.org/">web page</a><br>
</p>

<p>
<b>Matt Brubeck</b> is a primary Audacity developer.
He is in charge of the internationalization and localization
efforts for Audacity version 1.1, and has also contributed
many other features and bug fixes.  He graduated from Harvey Mudd
College in May, 2002 and is currently living in Vancouver, B.C.,
Canada.
<br>
<tt>email: mbrubeck <b>@</b> cs.hmc.edu</tt><br>
</p>

<h3>Active Developers</h3>

<p>
<b>Brian Gunlogson</b> is working on the adding completely
configurable keyboard shortcuts to Audacity 1.1.
<br>
<tt>email: bmg300 <b>@</b> yahoo.com</tt><br>
</p>

<p>
<b>Shane Mueller</b> wrote the new toolbar code for Audacity 1.1.
He is a graduate student in Psychology at the University of
Michigan.<br>
<tt>email: smueller <b>@</b> umich.edu</tt><br>
<a href="http://www-personal.umich.edu/~smueller/">web page</a>
</p>

<p>
<b>Greg Mekkes</b> wrote some of the functional enhancements for Audacity 1.1,
including selection format, snap-to, and align functions.
He is an aerospace engineer working at NASA Langley Research Center
in Hampton, VA.<br>
<tt>email: mekkes2020 <b>@</b> yahoo.com</tt><br>
</p>

<p>
<b>Augustus Saunders</b> is working on a project called
<i>libaudacity</i>, which will separate the core processing
code from the GUI of Audacity, resulting in a cross-platform
C++ library that could be easily incorporated into other programs.
He is currently consulting for Verilogix, Inc. in Torrance, CA.<br>
<tt>email: augustus.saunders <b>@</b> verilogix.net</tt><br>
</p>

<p>
<b>Tony Oetzmann</b> is the primary documentation writer for
Audacity.  He wrote most of the online help and tutorials,
and has also influenced the program design significantly.
He lives and works in Germany.
<br>
<tt>email: airon <b>@</b> epost.de</tt>
</p>

<h3>Other Notable Contributions</h3>

<p>
<b>Roger Dannenberg</b> wrote the sound file I/O,
MIDI file manipulation, and real-time audio I/O code for
Audacity version 1.0, and helped to develop the unique
algorithms which allow Audacity to edit files quickly
while maintaining unlimited Undo.  He is a Senior Research
Scientist at Carnegie Mellon University in Pittsburgh, PA.
<br>
<tt>email: rbd <b>@</b> cs.cmu.edu</tt><br>
<a href="http://www.cs.cmu.edu/~rbd/">web page</a><br>
</p>

<p>
<b>Paul Nasca</b> wrote many of the digital effects
included in Audacity, including BassBoost, Phaser, and Wahwah.
He studies Mathematics and Computer Science at
Petru Maior University in Targu-Mures, Romania.
<br>
<tt>email: paulnasca <b>@</b> email.ro</tt><br>
</p>

<p>
<b>Rob Leslie</b> is the author of
<a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>, the
MPEG/MP3 audio decoder that Audacity uses.  It also includes libid3tag,
which Audacity uses to decode and encode ID3 tags.  Besides writing
such excellent libraries, Rob also gives great support for them, and
even contributed a patch to Audacity when we were having trouble using
libid3tag.
</p>

<p>
<b>Ross Bencina</b> and <b>Phil Burk</b> are the primary authors of
<a href="http://www.portaudio.com/">portaudio</a>, the cross-platform
audio I/O toolkit that Audacity has adopted for version 1.1
</p>

<p>
<b>Harvey Lubin</b> of
<a href="http://www.agrapha.com">Agrapha</a> designed the main Audacity
logo and icons.
</p>

<p>
<b>Tom Woodhams</b> did the graphics for the Aqua toolbar for the Macintosh.
</p>

<p>
<b>Abe Milde</b> contributed several icons used in the edit toolbar.
</p>

<p>
<h3>Translators</h3>
</p>

<p>
<ul>

<li>Henrik Clausen (Danish, program and website)

<li>Ezequiel Plaza (Spanish, program and website)

<li>Yuri Ilyin (Russian, website)

<li>Alexandre Prokoudine (Russian, program)

<li>Lionel Allorge (French, program)

</ul>

</p>

<p>
<h3>Thanks</h3>

There are many other people and organizations we would like to thank.
Many of these have contributed code, bug reports, feature requests,
feedback, money, time, or other things to the project, and others have
simply produced tools which have made this whole thing possible.

<ul>

<li>Dave Beydler

<li>Jason Cohen

<li>Steve Harris

<li>Daniil Kolpakov

<li>Robert Leidle

<li>Logan Lewis

<li>Jason Pepas

<li>Mark Phillips

<li>Jonathan Ryshpan

<li>Patrick Shirkey

<li>Mark Tomlinson

<li>David Topper

<li>Rudy Trubitt

<li>The Ogg Vorbis Team

<li>The wxWindows Team

<li>Verilogix, Inc.

<li>(Let us know if we're forgetting anyone!)

</ul>

<?php BoxBottom();

include 'bottom.inc.php';

?>






