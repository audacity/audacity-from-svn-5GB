<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-c";
  $pageTitle = _("Audacity 1.3: Multiple clips per track");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("In Audacity 1.2, there is one audio 'clip' per track.  There is no easy way to time-shift part of a track without moving the rest.  In Audacity 1.3, you can split a single track into multiple clips.  You can move these clips around between different tracks, making it easy to construct complex compositions out of hundreds of smaller audio samples.")?></p>

<style type="text/css">
#amenu {
  float: left;
  border: 0;
  padding: 1em;
  padding-right: 4em;
}
#amenutitle {
  background:#000099;
  border:1px solid #666;
  margin:0px;
  width:3em;
  font-size:1em;
  font-weight:bold;
  color:#fff;
  padding:3px;
}
#amenulist {
  background:#ddd;
  border:1px solid #666;
  margin:0px;
  width:8em;
  position:relative;
  margin-top:-2px;
}
#amenulist ul {
  font-size:1em;
  margin:0;
  padding:0px;
  list-style:none;
}
#amenulist li, #amenulist h3 {
  display:block;
  border:1px solid;
  border-color:#eee #aaa #999 #eee;
  margin:0;
}
#amenulist :link, #amenulist :visited {
  display:block;
  text-decoration:none;
  padding:3px;
}
#amenulist :link:hover, #amenulist :visited:hover {
  background:#000099;
  color:#fff;
  text-decoration:none;
  padding:3px;
}
</style>

<div id="amenu">
<div id="amenutitle">Edit</div><div id="amenulist">
<ul>
<li>
&nbsp;...
</li>
<li>
<a href="#cut">Cut</a>
<a href="#splitcut">Split Cut</a>
<a href="#copy">Copy</a>
<a href="#paste">Paste</a>
<a href="#trim">Trim</a>
</li>
<li>
<a href="#delete">Delete</a>
<a href="#splitdelete">Split Delete</a>
<a href="#silence">Silence</a>
</li>
<li>
<a href="#split">Split</a>
<a href="#join">Join</a>
<a href="#duplicate">Duplicate</a>
</li>
<li>
&nbsp;...
</li>
</ul>
</div>
</div>

<p><?=_("The commands you will need to create and work with audio clips are found in the Edit menu.  Some of these commands have the same name as in Audacity 1.2, but their behavior has either changed or has been extended to support new functionality.")?></p>

<p><?=_("There are five ways to end up with multiple clips in a single track:")?></p>

<ol>
<li><?=_("Split in the middle of an existing clip.")?></li>
<li><?=_("Split Cut in the middle of an existing clip.")?></li>
<li><?=_("Split Delete in the middle of an existing clip.")?></li>
<li><?=_("Paste into an empty portion of an existing track.")?></li>
<li><?=_("Drag a clip from a different track.")?></li>
</ol>

<p><?=_("These commands, and other editing commands, are described in detail below.  Alternatively, click on the item in the menu to the left to jump directly to that command.")?></p>

<p><?=_("Other topics:")?></p>

<ul>
<li><a href="#movingbetween"><?=_("Moving clips between tracks")?></a></li>
<li><a href="#movingmultiple"><?=_("Moving multiple clips at once")?></a></li>
</ul>

<br clear="left">

<a name="cut">
<h3><?=_("Cut")?></h3>
  <p><?=_("Removes the selected audio and puts it on the clipboard, and shifts the following audio to the left.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips002.png">
  </p>
</a>

<a name="splitcut">
<h3><?=_("Split Cut")?></h3>
  <p><?=_("Removes the selected audio and puts it on the clipboard, but does not shift the following audio.  The preceding and following audio are now separate clips that can be moved independently.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips003.png">
  </p>
</a>

<a name="copy">
<h3><?=_("Copy")?></h3>
  <p><?=_("Copies the selected audio to the clipboard.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips001.png">
  </p>
</a>

<a name="trim">
<h3><?=_("Trim")?></h3>
  <p><?=_("Removes all audio from the current clip except the selected part.  Does not affect other clips in the same track.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips004.png">
  </p>
</a>

<a name="delete">
<h3><?=_("Delete")?></h3>
  <p><?=_("Removes the selected audio without placing it on the clipboard, and shifts following audio to the left.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips002.png">
  </p>
</a>

<a name="splitdelete">
<h3><?=_("Split Delete")?></h3>
  <p><?=_("Removes the selected audio without placing it on the clipboard, but does not shift the following audio.  The preceding and following audio are now separate clips that can be moved independently.")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips003.png">
  </p>
</a>

<a name="silence">
<h3><?=_("Silence")?></h3>
  <p><?=_("Replaces the selection with silence")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips005.png">
  </p>
</a>

<a name="split">
<h3><?=_("Split")?></h3>
  <p><?=_("Splits the current clip into up to three clips at the selection boundaries.  The audio before, within, and after the selection can now all be shifted independently")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips006.png"><br>
    <img src="images/clips007.png">
  </p>
</a>

<a name="duplicate">
<h3><?=_("Duplicate")?></h3>
  <p><?=_("Creates a new track containing only the current selection as a new clip")?></p>
  <p align="center">
    <img src="images/clips001.png"><br>
    <img src="images/clips008.png">
  </p>
</a>

<a name="paste">
<h3><?=_("Paste - into an existing clip")?></h3>
  <p><?=_("When you select Paste and the cursor is inside a clip, the audio gets inserted into the middle of the clip and the following audio is shifted to the right to make room")?></p>
  <p align="center">
    <img src="images/clips009.png"><br>
    <img src="images/clips010.png">
  </p>

<h3><?=_("Paste - into the empty part of a track")?></h3>
  <p><?=_("When you select Paste and the cursor is outside a clip, and there is enough room for the audio on the clipboard, the audio on the clipboard is inserted without any other clips being shifted over")?></p>
  <p align="center">
    <img src="images/clips011.png"><br>
    <img src="images/clips012.png">
  </p>
</a>

<a name="join">
<h3><?=_("Join")?></h3>
  <p><?=_("If you select a region that overlaps one or more clips, they are all joined into one large clip.  Regions in-between clips are treated as silence.")?></p>
  <p align="center">
    <img src="images/clips013.png"><br>
    <img src="images/clips014.png">
  </p>
</a>

<a name="movingbetween">
<h3><?=_("Moving clips between tracks")?></h3>
  <p><?=_("The time-shift tool can now be used to move a single clip between tracks.  Just click and drag.")?></p>
  <p align="center">
    <img src="images/clips015.png"><br>
    <img src="images/clips016.png">
  </p>
</a>

<a name="movingmultiple">
<h3><?=_("Moving multiple clips at once")?></h3>
  <p><?=_("To time-shift multiple clips at once, select all of the clips you want to move using the normal selection tool, then use the time-shift tool to click and drag.  If you click within the selection, all selected clips will move together.  If you click outside the selection, only the clip under the mouse cursor will move.")?></p>
  <p align="center">
    <img src="images/clips017.png"><br>
    <img src="images/clips018.png">
  </p>
</a>

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
<a href="features-1.3-b.php">
<?=_("Back: ")?>
<?=_("Collapse/Expand Tracks")?>
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
