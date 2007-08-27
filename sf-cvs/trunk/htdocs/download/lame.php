<?php
/*
 * Copyright 2007 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "lame";
  $pageTitle = _("LAME MP3 Encoder");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php printf(_('<p>Because of software patents, we cannot distribute MP3 encoding software ourselves.  Follow these instructions to use the free LAME encoder to export MP3 files with Audacity.
<h3>Windows</h3>
<ol>
  <li>Go to the <a href="%s">LAME download page</a>.</li>
  <li>Click on any link from the list of identical "%s" links.</li>
  <li>When you have finished downloading LAME, unzip it and save the file <b>lame_enc.dll</b> anywhere on your computer.</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask you where lame_enc.dll is saved.</li>
</ol>
<h3>Mac OS 9 or X</h3>
<ol>
  <li>Go to the <a href="%s">LAME download page</a>.</li>
  <li>Download the version of LameLib for your operating system.</li>
  <li>When you have finished downloading, use Stuffit Expander to extract the files.  (This may happen automatically.)</li>
  <li>Save the file called "LameLib" anywhere on your computer.</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask you where LameLib is saved.</li>
</ol>'), "http://lame.buanzo.com.ar/", "lame-3.96.1", "http://spaghetticode.org/lame/") ?>

<?php
  include "../include/footer.inc.php";
?>
