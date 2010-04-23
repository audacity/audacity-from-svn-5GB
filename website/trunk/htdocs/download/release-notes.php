<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 - 2010 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "release-notes";
  $pageTitle = _("Release Notes");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Known Problems")?></h3>
<p><?php printf(_("The following were known bugs or problems in Audacity %s at release:"), src_version); ?></p>
<ul>
  <li><p><?=_("Audacity can import and display MIDI files, but cannot play or edit them.")?></p></li>
  <li><p><?=_("Linux:  Recording in full duplex (playing existing tracks while recording) on some systems causes mono recordings to sound distorted or low-pitched.  To work around this problem, set Audacity to record in stereo.")?></p></li>
  <li><p><?=_("MacOS X: Audacity cannot work with files or folders that are contained inside folders with non-English characters (accents, symbols, etc.) in their names.  Files with accented characters work, and Audacity projects with accented characters work.  Only folders with accented characters in their names will cause problems.")?></p></li>
</ul>

<p><?php printf(_("The following additional problems have become apparent in Audacity %s after release:"), src_version); ?></p>
<ul>
  <li><p><?=_("Windows Vista: Pressing Stop after recording may cause a crash.")?></p></li>
  <li><p><?=_("Mac OS X: Shortcut keys may activate when typing in file open or save windows.")?></p></li>
  <li><p><?=_("<a href=\"../download\">Audacity Beta (1.3 Series)</a> is now the recommended version on Windows 7, Windows Vista and Mac OS X 10.6.")?></p></li>
</ul>

<?=_("<h3>Changes in Audacity 1.2.6</h3>
<ul>
<li>Fixed memory leaks on Windows.
</li>

<li>Corrected font size problems.
</li>

<li>FLAC import now works correctly. FLAC export does not work on Windows or Mac.
</li>

<li>Fixed Mac OS X (PPC only) screen redraw issues.
</li>")?>

</ul>

<?=_("<h3>Changes in Audacity 1.2.5</h3>
<ul>
<li>An official Intel Mac version is now available.
</li>
<li>Fixed bug in Generate Silence which caused it to apply to all tracks
    instead of just the selected ones.
</li>
<li>Mac OS X: audio device opening code has been rewritten.  First, it
    is much more conservative about changing device settings; it will
    not change settings when you open the program or close the
    preferences dialog anymore, and it will not change the settings when
    you begin playback/recording if the current settings are adequate.
    When it does change the settings, it should work much better on
    devices such as the Griffin iMic, ART USB Phono Plus,
    and Ion iMX02 USB.
</li>
<li>Mac OS X: added new Audio I/O preference that lets you tell Audacity
    to never change any audio device settings.
</li>
<li>Newer libsndfile supports FLAC import and export
</li>
<li>Updated soundtouch to current version which is faster and better quality
</li>
<li>Modified configure script prefers system libraries to local copies to
    reduce compilation times and memory usage.
</li>
<li>Minor updates to help files.
</li>
<li>New or updated translations: Bulgarian (bg), Galician (gl),
    Traditional Chinese (zh_TW), Simplified Chinese (zh), Slovenian (sl),
    Swedish (sv), Bangladeshi (bn), Slovakian (sk), Romanian (ro),
    Lithuanian (lt), Welsh (cy), and Turkish (tr).
</li>
</ul>")?>

<?=_("<h3>Changes in Audacity 1.2.4b</h3>
<ul>
<li>A serious problem with the French translation was corrected.
</ul>")?>

<?=_("<h3>Changes in Audacity 1.2.4</h3>
<ul>
<li>The File menu now includes a list of recent files.

</li>
<li>The \"Generate Silence\" effect now prompts for a length.

</li>
<li>Audacity is now built with Vorbis 1.1, which features better encoding
    quality and file compression.

</li>
<li>Dragging sound files into the Audacity window now works on Mac OS X
    and Linux, as well as Windows.  (Before, it worked only on Windows.)

</li>
<li>Better support for certain audio devices on Mac OS X 10.4 \"Tiger\"

</li>
<li>The \"View History\" window can now discard old undo levels to save disk
    space on Windows.  (This previously worked only on Linux and Mac.)

</li>
<li>\"Preferences\" command is now in Edit menu.

</li>
<li>\"Plot Spectrum\" command is now in Analyze menu.

</li>
<li>Opening a project file saved by a later version of Audacity displays
    an intelligent error message.  Also, trying to import a project file
    (instead of open it) displays an intelligent error message.

</li>
<li>Audacity now compiles in Visual C++ .NET 2003.

</li>
<li>Other minor bug fixes.

</li>
<li>New or updated translations: Arabic (ar), Czech (cs), Finnish (fi),
    Hungarian (hu), Japanese (ja), Norwegian (nb), Slovenian (sl),
    Simplified Chinese (zh_CN), Traditional Chinese (zh_TW).
</li>
</ul>")?>

<?php
  include "../include/footer.inc.php";
?>
