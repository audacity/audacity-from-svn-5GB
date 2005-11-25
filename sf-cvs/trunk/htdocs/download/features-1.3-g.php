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

<p><?=_("Better performance with large projects")?></p>

<p><?=_("Project integrity check on open")?></p>

<p><?=_("Transcription toolbar")?></p>

<p><?=_("Upload (?)")?></p>

<p><?=_("Batch")?></p>

<p><?=_("Cut lines")?></p>

<p><?=_("CleanSpeech")?></p>

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
