<?php
/*
 * Copyright 2005 Dominic Mazzoni
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

<p><b><?=_("When you open an Audacity project in version 1.3, it checks to make sure that all data files are present.")?></b></p>

<p><b><?=_("Transcription toolbar")?></b></p>

<p><?=_("A new toolbar for creating transcriptions of speech in Label Tracks.  Includes features that enable you to change the playback speed, create labels automatically based on onsets, and more.  To enable this feature, go to the Interface tab of the Preferences.")?></p>

<p><b><?=_("Upload via FTP")?></b></p>

<p><?=_("Audacity 1.3 has an experimental new dialog for uploading files to a server via FTP.  Currently it is just a simple FTP client; in the future we will integrate this feature into Audacity more so you can publish a project directly to a site as a Podcast, for example.")?></p>

<p><b><?=_("Based on wxWidgets 2.6")?></b></p>

<p><?=_("Previous versions of Audacity were based on wxWidgets 2.4.  This change brings support for GTK2 widgets on Unix and better Unicode support on all platforms, plus many other minor enhancements under the hood.")?></p>

<p><b><?=_("Batch / CleanSpeech")?></b></p>

<p><?=_("Audacity has a new feature that allows you to process a bunch of files, for example normalizing and converting to MP3.  The CleanSpeech mode provides a simplified interface for some standard adjustments typically made on speech recordings.  You can access these features in the Batch tab of the Preferences dialog.")?></p>

<p><b><?=_("Cut lines")?></b></p>

<p><?=_("When this feature is enabled (in the Interface tab of the Preferences), cutting audio in the middle of a clip does not remove it permanently - at any time in the future, just click on the cut line to restore it.")?></p>

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
<a href="features-1.3-f.php">
<?=_("Back: ")?>
<?=_("QuickTime and Audio Units on Mac OS X")?>
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
