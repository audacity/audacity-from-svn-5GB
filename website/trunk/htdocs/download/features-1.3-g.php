<?php
/*
 * Copyright 2005 - 11 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-g";
  $pageTitle = _("Audacity 1.3: Other features");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><b><?=_("Better performance with large projects")?></b></p>

<p><?=_("As before, Audacity stores projects using a directory full of small files.  Audacity 1.3 now uses a hierarchical directory structure that results in better performance on large projects (tens of hours worth of audio across many tracks).")?></p>

<p><b><?=_("Project integrity check on open")?></b></p>

<p><?=_("When you open an Audacity project in version 1.3, it checks to make sure that all data files are present.")?></p>

<p><b><?=_("Upload via FTP")?></b></p>

<p><?=_("Up to and including Audacity 1.3.3, an experimental dialog was available for uploading files to a server via FTP. A future version of Audacity may re-introduce this feature as an optional plug-in.")?></p>

<p><b><?=_("Based on wxWidgets 2.8")?></b></p>

<p><?=_("Previous versions of Audacity were based on wxWidgets 2.4.  This change brings support for GTK2 widgets on Unix and better Unicode support on all platforms, plus many other minor enhancements under the hood.")?></p>

<p><b><?=_("Batch / CleanSpeech")?></b></p>

<p><?=_("Audacity has a new feature that allows you to process a batch of files, for example normalizing and converting to MP3.  CleanSpeech mode provides a simplified interface for some standard adjustments typically made on speech recordings.  You can access these features in the Interface Preferences.")?></p>

<p><b><?=_("Cut lines")?></b></p>

<p><?=_("Turning this feature on in the Tracks Preferences means that cutting audio in the middle of a clip does not remove it permanently - click on the cut line at any time to restore it.")?></p>

<style type="text/css">
.navbox {
  background:#eeeeff;
  border:1px solid;
  border-color:#eee #aaa #999 #eee;
  padding:0.5em;
  margin:0.5em;
}
</style>

<p>
<span class="navbox">
<a href="features-1.3-e.php">
<?=_("Back: ")?>
<?=_("Improved Label Tracks")?>
</a>
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
