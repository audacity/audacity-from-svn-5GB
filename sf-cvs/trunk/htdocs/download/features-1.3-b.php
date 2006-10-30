<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-b";
  $pageTitle = _("Audacity 1.3: Collapse/Expand Tracks");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("In Audacity 1.3, every track has an upward-pointing triangle at the bottom of the label area on the left side of the track.  Click it once to collapse the track to a very small size, and click it again to expand it back to its previous size.  Note that as in previous versions, you can resize a track by clicking anywhere along the bottom edge and dragging up or down.")?></p>

<p align="center">
<img src="images/collapse001.png">
<br>
<?=_("Before clicking on the collapse button for the top track")?>
</p>

<p align="center">
<img src="images/collapse002.png">
<br>
<?=_("After clicking on the collapse button")?>
</p>

<p align="center">
<img src="images/collapse003.png">
<br>
<?=_("After selecting 'Collapse All Tracks' from the View menu")?>
</p>

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
<?=_("Mac OS X features")?>
</span>

<span class="navbox">
<a href="features-1.3-c.php">
<?=_("Next: ")?>
<?=_("Multiple clips per track")?>
</span>
</p>

<span class="navbox">
<a href="features-1.3-a.php">
<?=_("Top: ")?>
<?=_("New features in Audacity 1.3")?>
</a>
</span>

<?php
  include "../include/footer.inc.php";
?>
