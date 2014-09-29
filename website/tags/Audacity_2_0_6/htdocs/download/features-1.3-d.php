<?php
/*
 * Copyright 2005 - 2010 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-d";
  $pageTitle = _("Audacity 1.3: Selection Toolbar");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p align="center">
<img src="images/new-sel-bar-1.png">
<br>
<?=_("The selection Toolbar")?>
</p>

<p>
<?=_("In Audacity 1.2, the current selection is contained in a status bar at the bottom of the window.  In Audacity 1.3, this is replaced by a fully functional Selection Toolbar, which displays and controls the current selection (your choice of Start and End, or Start and Length), and the current audio position.  Selection Toolbar is fully editable - just click in any field and type to change the current selection precisely.  In addition, many formatting options allow you to view times in different units, such as samples, CD frames, or NTSC video frames.")?>
</p>

<p align="center">
<img src="images/selbar003.png">
<br>
<?=_("The project sample rate control on Selection Toolbar")?>
</p>

<p align="center">
<img src="images/new-sel-bar-2.png">
<br>
<?=_("Typing a new time directly into Selection Toolbar.")?>
</p>

<p align="center">
<img src="images/selbar006b.png">
<br>
<?=_("Different time formats supported by Selection Toolbar")?>
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
<a href="features-1.3-l.php">
<?=_("Back: ")?>
<?=_("Project saving and recovery")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-f.php">
<?=_("Next: ")?>
<?=_("Mac OS X features")?>
</span>

<?php
  include "../include/footer.inc.php";
?>
