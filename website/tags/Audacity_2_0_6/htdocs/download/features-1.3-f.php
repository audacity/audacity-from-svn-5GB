<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-f";
  $pageTitle = _("Audacity 1.3: Mac OS X features");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<!-- Commented out because this code is not actually checked in!  :(
On Mac OS X, Audacity can now import any audio file supported by Apple's QuickTime technology.  This includes .MOV and .MP4 (AAC) files.  Unfortunately encrypted audio files (such as those from the iTunes Music Store) cannot be imported directly into Audacity - Apple does not allow this to be done easily because it would be too easy to circumvent the encryption this way. -->

<p><?=_("Audacity's audio I/O library, PortAudio v19, now supports many more Mac audio devices, including multichannel devices that were not supported before.")?></p>

<p><?=_("Audacity is now available for Intel Macs.  For now, this is being provided as a separate download, rather than a universal binary.")?></p>

<p><?=_("Also on Mac OS X, Audacity now supports Audio Unit plug-ins.  Audacity searches for Audio Units in the usual location, in the system or user's Library folder.")?></p>

<style type="text/css">
.navbox {
  background:#eeeeff;
  border:1px solid;
  border-color:#eee #aaa #999 #eee;
  padding:0.5em;
  margin:0.5em;
}
</style>

<p><i><?=_("This is the end of new features in Audacity 1.3.1 and 1.3.2.  Continue on to read about features that were previously introduced in Audacity 1.3.0.")?></i></p>

<span class="navbox">
<a href="features-1.3-d.php">
<?=_("Back: ")?>
<?=_("Selection Bar")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-b.php">
<?=_("Next: ")?>
<?=_("Collapse/Expand Tracks")?>
</span>
</p>

<p>
<span class="navbox">
<a href="features-1.3-a.php">
<?=_("Top: ")?>
<?=_("New features in Audacity 1.3")?>
</a>
</span>
</p>

<?php
  include "../include/footer.inc.php";
?>
