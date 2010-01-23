<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-e";
  $pageTitle = _("Audacity 1.3: Improved Label Tracks");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Label Tracks are Audacity's way for you to create markings and annotations within your project.  In Audacity 1.3, Label Tracks are much improved, with support for overlapping labels, and support for modifying both the left and right edge of the label region just by clicking and dragging.")?></p>

<p align="center">
<img src="images/label001.png">
<br>
<?=_("To create a label, start by selecting some audio.")?>
</p>

<p align="center">
<img src="images/label002.png">
<br>
<?=_("Select 'Add Label At Selection' from the Tracks menu.")?>
</p>

<p align="center">
<img src="images/label003.png">
<br>
<?=_("Type some text into the label.")?>
</p>

<p align="center">
<img src="images/label004.png">
<br>
<?=_("Click and drag the handles to resize the label.")?>
</p>

<p align="center">
<img src="images/label005.png">
<br>
<?=_("Later, you can retrieve the selection - just click inside the label.")?>
</p>

<p align="center">
<img src="images/label006.png">
<br>
<?=_("After you click, the label region becomes the current selection, allowing you to perform an operation on other tracks in the area within the label.")?>
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

<span class="navbox">
<a href="features-1.3-c.php">
<?=_("Back: ")?>
<?=_("Multiple clips per track")?>
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
