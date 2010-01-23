<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-d";
  $pageTitle = _("Audacity 1.3: Selection Bar");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p align="center">
<img src="images/selbar001b.png">
<br>
<?=_("The selection bar")?>
</p>

<p>
<?=_("In Audacity 1.2, the current selection is contained in a status bar at the bottom of the window.  In Audacity 1.3, this is replaced by a fully functional Selection Bar, which displays and controls the current selection (your choice of Start and End, or Start and Length), and the current audio position.  The selection bar is fully editable - just click in any field and type to change the current selection precisely.  In addition, many formatting options allow you to view times in different units, such as samples, CD frames, or NTSC video frames.")?>
</p>

<p align="center">
<img src="images/selbar003.png">
<br>
<?=_("The project sample rate control on the selection bar")?>
</p>

<p align="center">
<img src="images/selbar004.png">
<br>
<?=_("The current selection on the selection bar")?>
</p>

<p align="center">
<img src="images/selbar005.png">
<br>
<?=_("The audio position on the selection bar")?>
</p>

<p align="center">
<img src="images/selbar006.png">
<br>
<?=_("Different time formats supported by the selection bar")?>
</p>

<style type="text/css">
.navbox {
  background:#eeeeff;
  border:1px solid;
  border-color:#eee #aaa #999 #eee;
  padding:0.5em;
  margin:0.5em;}
</style>

<span class="navbox">
<a href="features-1.3-c.php">
<?=_("Back: ")?>
<?=_("Multiple clips per track")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-e.php">
<?=_("Next: ")?>
<?=_("Improved Label Tracks")?>
</span>

<?php
  include "../include/footer.inc.php";
?>
