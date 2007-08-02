<?php
/*
 * Copyright 2005 Dominic Mazzoni
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
<?php include "betawarning.inc.php" ?>

<h3><?=_("New features in Audacity 1.3.3")?></h3>
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
