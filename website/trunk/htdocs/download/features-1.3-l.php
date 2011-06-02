<?php
/*
 * Copyright 2006 - 11 Dominic Mazzoni, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-l";
  $pageTitle = _("Audacity 1.3: Project saving and recovery");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Starting in version 1.3.2, Audacity automatically saves snapshots
of your current project constantly.  If Audacity ever crashes,
or if your whole computer crashes or suddenly loses power, the next time
you start Audacity it will notify you that it found an unsaved project,
and prompt you to recover it.
")?></p>

<p><?=_("One common source of confusion with Audacity is that when you
open or import an uncompressed file (e.g. WAV or AIFF), Audacity by 
default saves time and memory by only linking to the file, not copying it.
But if you move, rename or delete the original file, your Audacity project
will not work anymore.  Starting with version 1.3.2, Audacity prompts
the first time you save your project, and on exiting, if the project 
depends on other audio files. If there is a dependency, Audacity gives 
you the option of copying the required files into the project, making
it self-contained.")?></p>

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
<a href="features-1.3-k.php">
<?=_("Back: ")?>
<?=_("Timer Recording")?>
</a>
</span>

<span class="navbox">
<a href="features-1.3-d.php">
<?=_("Next: ")?>
<?=_("Selection Bar")?>
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
