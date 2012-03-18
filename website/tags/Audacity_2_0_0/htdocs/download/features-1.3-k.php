<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-k";
  $pageTitle = _("Audacity 1.3: Timer Recording");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Timer Recording lets you schedule Audacity to start recording at a particular time and record only for a certain length of time.  Perfect if you want to record Internet radio or walk away while Audacity is recording from a cassette or LP.")?></p>

<p align="center">
<img src="images/timer-record-1.png">
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
<a href="features-1.3-j.php">
<?=_("Back: ")?>
<?=_("Repair and Equalization effects")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-l.php">
<?=_("Next: ")?>
<?=_("Project saving and recovery")?>
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
