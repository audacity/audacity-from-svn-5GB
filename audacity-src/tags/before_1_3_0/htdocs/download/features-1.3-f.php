<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-f";
  $pageTitle = _("Audacity 1.3: QuickTime and Audio Units on Mac OS X");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("On Mac OS X, Audacity can now import and audio file supported by Apple's QuickTime technology.  This includes .MOV and .MP4 (AAC) files.  Unfortunately encrypted audio files (such as those from the iTunes Music Store) cannot be imported directly into Audacity - Apple does not allow this to be done easily because it would be too easy to circumvent the encryption this way.")?></p>

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

<span class="navbox">
<a href="features-1.3-e.php">
<?=_("Back: ")?>
<?=_("Improved Label Tracks")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-g.php">
<?=_("Next: ")?>
<?=_("Other features")?>
</span>

<?php
  include "../include/footer.inc.php";
?>
