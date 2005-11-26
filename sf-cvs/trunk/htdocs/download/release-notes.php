<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "release-notes";
  $pageTitle = _("Release Notes");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Known Problems")?></h3>
<p><?php printf(_("The following are known bugs or problems in Audacity %s:"), src_version); ?></p>
<ul>
  <li><p><?=_("Audacity can import and display MIDI files, but cannot play or edit them.")?></p></li>
  <li><p><?=_("Linux:  Recording in full duplex (playing existing tracks while recording) on some systems causes mono recordings to sound distorted or low-pitched.  To work around this problem, set Audacity to record in stereo.")?></p></li>
  <li><p><?=_("MacOS X: Audacity cannot work with files or folders that are contained inside folders with non-English characters (accents, symbols, etc.) in their names.  Files with accented characters work, and Audacity projects with accented characters work.  Only folders with accented characters in their names will cause problems.")?></p></li>
</ul>

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
