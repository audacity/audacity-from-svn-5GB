<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-j";
  $pageTitle = _("Audacity 1.3: Repair and Equalization Effects");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=_("Repair Effect")?></h2>

<p><?=_("This new effect will fix a small pop or click in an audio track by smoothly interpolating the damaged audio samples.  It is designed to fix very short glitches, not major distortion or background noise.  It can fix minor clipping.")?></p>

<p align="center">
<img src="images/repair-1.png">
<br>
<?=_("First, find the damanged part of your recording and zoom in<br>
until you can see the individual audio samples.")?>
</p>

<p align="center">
<img src="images/repair-2.png">
<br>
<?=_("Select the damaged portion of the audio.")?>
</p>

<p align="center">
<img src="images/repair-3.png">
<br>
<?=_("Choose <b>Repair</b> from the Effect menu.<br>The selection is replaced with smoothly interpolated samples.")?>
</p>

<h2><?=_("Equalization Effect")?></h2>

<p><?=_("The new Equalization effect, shown below, has many more controls
for constructing a complex filter to enhance or reduce certain frequencies.
You can either draw an arbitrary equalization curve, or use a graphic
equalizer.
")?></p>

<p align="center">
<img src="images/equalization-1.png">
<br>
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
<a href="features-1.3-i.php">
<?=_("Back: ")?>
<?=_("Track focus")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-k.php">
<?=_("Next: ")?>
<?=_("Timer Recording")?>
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
