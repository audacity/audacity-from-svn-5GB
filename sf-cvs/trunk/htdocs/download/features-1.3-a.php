<?php
/*
 * Copyright 2005-2008 Dominic Mazzoni, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-a";
  $pageTitle = _("New features in Audacity 1.3");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_('Beta version')?></h3>
<p>
<?=_('Audacity 1.3 Beta is our new, not quite finished, development version which will be the basis of our next "stable" version.')?></p>

<p><?=_('
 <ul>
  <li>Contains dozens of new, exciting features. Very occasionally, these might need final polishing or not be retained in later versions.</li>
  <li>Occasionally, a feature might not work as it did before, or might be temporarily disabled.</li>
  <li>Some parts of the program are not yet documented or translated into different languages.</li>
 </ul>
')?></p>

<h3 id="details"><?=_("New in Audacity 1.3.4")?></h3>
<dl>
  <dt></dt>
  <dd>
    <!-- indent cheat -->
    <dl>
      <dt> New features </dt>
      <dd>New Welcome Screen with introduction to Audacity.</dd>
      <dd>Enhanced Windows Shell integration, so Audacity shows up in lots of
          Windows places such as "Open With".</dd>
      <dd>New keyboard command: 'Mix and Render to New Track' (bound to Ctrl+Shift+M).</dd>
      <dd>New keyboard shortcut: "Shift-A" starts playback when stopped,
          or performs "Stop and Select" when playing.</dd>
      <dd>Added support for VAMP audio analysis plug-ins.</dd>
      <dd>Solo button solos only one track at a time, and a track cannot be both mute and solo.</dd>

      <dt> Interface </dt>
      <dd>Keyboard shortcuts for making short/long jumps along the timeline.</dd>
      <dd>Added 'Snap To' in the Selection Bar.</dd>
      <dd>Made keyboard navigation easier when multiple menu items with the same first letter exist.</dd>
      <dd>Enhanced interface for label editing.</dd>
      <dd>Layout of OK/Cancel buttons consistency improved.</dd>
      <dd>
        Preferences:
        <ul>
          <li>"Select all audio in project, if none selected" (on by default).</li>
          <li>"Beep on completion of longer activities" (system bell, not main output).</li>
          <li>Other preferences cleaned up and explanations improved.</li>
        </ul>
      </dd>
      <dd>Envelopes: Many fixes when copying / pasting / repeating.</dd>
      <dd>Many translation updates.</dd>
      <dd>Track height fixed in several cases.</dd>
      <dd>CleanSpeech mode switching without closing and re-opening fixed.</dd>

      <dt> Opening/Saving Formats </dt>
      <dd>Metadata editor added for OGG, FLAC and WAV/AIFF exports, and general improvements.</dd>
      <dd>Metadata import improved.</dd>
      <dd>Muted tracks are no longer audible in the exported mix.</dd>

      <dt> Effects </dt>
      <dd>Truncate Silence: support for multiple and stereo tracks.</dd>
      <dd>
        Dtmf Generator:
        <ul>
          <li>Added support for keypad letters.</li>
          <li>Added an amplitude control.</li>
        </ul>
      </dd>
      <dd>Compressor: variable decay time added.</dd>
      <dd>
        Equalization:
        <ul>
          <li>Clicks at start / end prevented.</li>
          <li>Improvements to saved curves being found.</li>
          <li>Preview works correctly.</li>
        </ul>
      </dd>
      <dd>'Merge' command appears in Undo history.</dd>
      <dd>Clipping detected more reliably.</dd>
      <dd>Nyquist plug-ins reviewed and enhanced.</dd>
      <dd>Better (and more) progress bars.</dd>
      <dd>Cancelling effect always restores previous audio.</dd>
      <dd>Several improvement to effects in batch mode.</dd>

      <dt> Recording/Playback </dt>
      <dd>Improvements to latency correction.</dd>
      <dd>Updated version of portaudio-v19 library.</dd>

      <dt>
        Note that Help is no longer built in, but accessible on the Web via links in Audacity.
      </dt>
    </dl>
  </dd>
</dl>

<h3 id="olderdetails"><?=_("New features in Audacity 1.3.3")?></h3>
<dl>
  <dt></dt>
  <dd>
    <!-- indent cheat -->
    <dl>
      <dt> Opening/saving formats </dt>
      <dd>
        Import
        <ul>
          <li>Import of audio from QuickTime (mov, aac, m4a) files now supported on OSX.</li>
          <li>Broadcast Wave Format (BWF) wave files can now be imported.</li>
        </ul>
      </dd>
      <dd>
        Export
        <ul>
          <li>Metadata can be added to OGG files.</li>
          <li>Improved Export option selection.</li>
          <li>Additional export options added to MP3 and FLAC file formats.</li>
          <li>Command line exporter now supported on Windows and OSX.</li>
        </ul>
      </dd>
      <dt> Effects </dt>
      <dd>
        EQ effect
        <ul>
          <li>Responsiveness improved.</li>
          <li>Several enhancements added.</li>
          <li>Batch support added.</li>
        </ul>
      </dd>
      <dd> New Auto Duck effect </dd>
      <dd> Added previewing to AudioUnit effects. </dd>
      <dd> Much improved Noise Removal effect </dd>
      <dd> Effects previewing can now be canceled. </dd>
      <dd> New DTMF Tone Generator effect </dd>
      <dd> Additional options available in Noise effect. </dd>
      <dd> Improved the Tone Generation effects. </dd>
      <dt> Other features </dt>
      <dd> New built-in screen capture utility </dd>
      <dd> Major speed improvement in Spectrogram rendering </dd>
      <dd> Increased support for drag and drop on OSX. </dd>
      <dd> Support added for building against wxWidgets 2.8.x. </dd>
      <dd> Can now open multiple Audacity Projects at once from Explorer on Windows. </dd>
      <dd> Improved main window sliders. </dd>
      <dd> New support for snapping while selecting and sliding </dd>
      <dd> Improved track focus handling and visual feedback. </dd>
      <dd> Speed improvements and handling of resizing/zooming in tracks </dd>
      <dd> Spectrum view can now be zoomed. </dd>
      <dd> New internal file cache to improve handling of project files over networks </dd>
      <dt> Also </dt>
      <dd> Many improvements to language specific translations </dd>
      <dd> Numerous stability improvements </dd>
    </dl>
  </dd>
</dl>

<h3><?=_("New features in Audacity 1.3.1 and 1.3.2")?></h3>

<ol>
<li><a href="features-1.3-h.php"><?=_("Improved Toolbar Docking")?></a></li>
<li><a href="features-1.3-i.php"><?=_("Track focus for improved keyboard navigation")?></a></li>
<li><a href="features-1.3-j.php"><?=_("Repair and Equalization effects")?></a></li>
<li><a href="features-1.3-k.php"><?=_("Timer Recording")?></a></li>
<li><a href="features-1.3-l.php"><?=_("Project saving and recovery")?></a></li>
<li><a href="features-1.3-d.php"><?=_("Selection Bar")?></a></li>
<li><a href="features-1.3-f.php"><?=_("Mac OS X features")?></a></li>
</ol>

<h3><?=_("New features in Audacity 1.3.0")?></h3>

<ol>
<li><a href="features-1.3-b.php"><?=_("Collapse/Expand Tracks")?></a></li>
<li><a href="features-1.3-c.php"><?=_("Multiple clips per track")?></a></li>
<li><a href="features-1.3-e.php"><?=_("Improved Label Tracks")?></a></li>
<li><a href="features-1.3-g.php"><?=_("Other features")?></a></li>
</ol>

<?php
  include "../include/footer.inc.php";
?>
