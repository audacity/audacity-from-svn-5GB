<?php
/*
 * Copyright 2006 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-h";
  $pageTitle = _("Audacity 1.3: Improved Toolbar Docking");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("In Audacity 1.3.2 and higher, you can easily dock, undock, and rearrange the location of all of Audacity's toolbars, including the selection bar.  A new blue triangle visually indicates when you are about to dock a toolbar in a particular location.")?></p>

<p align="center">
<img src="images/toolbar-drag-1.png">
<br>
<?=_("First, click on the grabber to the left of a toolbar you wish to drag.")?>
</p>

<p align="center">
<img src="images/toolbar-drag-2.png">
<br>
<?=_("Drag the toolbar away from its original location.<br>If you release the mouse here, it will become a floating toolbar.")?>
</p>

<p align="center">
<img src="images/toolbar-drag-3.png">
<br>
<?=_("While dragging, position the mouse near an edge or between two other toolbars,<br>and a blue triangle appears, indicating a possible location you can dock the toolbar.")?>
</p>

<p align="center">
<img src="images/toolbar-drag-4.png">
<br>
<?=_("Release the mouse button near a blue arrow and the toolbar is docked in its new location.")?>
</p>

<p><?=_("Commands to hide or show toolbars are found in the View menu.")?></p>

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
<a href="features-1.3-i.php">
<?=_("Next: ")?>
<?=_("Track focus")?>
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
