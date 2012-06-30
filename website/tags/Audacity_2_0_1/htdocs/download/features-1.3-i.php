<?php
/*
 * Copyright 2006 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-i";
  $pageTitle = _("Audacity 1.3: Track focus");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("In Audacity 1.3.2 and higher, you can fully manipulate the
selection using the keyboard.  As always, press the left and right
arrows, along with shift and control, to move the cursor or extend a
selection to the left or right.  New, press the up or down arrows
to change which track is focused, independent of the selection, and
press the Enter key to toggle the selectedness of the focused track.
")?></p>

<p><?=_("The focused track is highlighted in green, as illustrated in
the screenshots below.  Clicking on a track selects it and focuses it.
")?></p>

<p align="center">
<img src="images/focus-1.png">
<br>
<?=_("Above, a region of audio is selected in the top track.<br>
You can tell the track is selected, because its label area is shaded.<br>
You can tell the track is focused because of the green border.")?>
</p>

<p align="center">
<img src="images/focus-2.png">
<br>
<?=_("Pressing the down arrow key changes the focus to the lower track.<br>
However, only the top track is selected.")?>
</p>

<p align="center">
<img src="images/focus-3.png">
<br>
<?=_("Pressing the enter key toggles the lower track, so it is now selected, too.")?>
</p>

<p align="center">
<img src="images/focus-4.png">
<br>
<?=_("Finally, press up-arrow and enter to unselect the top track.<br>
This is one way to keep a particular range of audio selected<br>while changing which track is selected.")?>
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
<a href="features-1.3-h.php">
<?=_("Back: ")?>
<?=_("Improved Toolbar Docking")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-j.php">
<?=_("Next: ")?>
<?=_("Repair and Equalization effects")?>
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
