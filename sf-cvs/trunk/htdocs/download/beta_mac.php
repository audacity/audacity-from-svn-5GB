<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2008 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "beta_mac";
  $pageTitle = _("Mac OS X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_('Beta version')?></h3>
<a href="#sysreq">System Requirements</a> &nbsp; <a href="#recdown">Download</a>
<p>
<?=_('Audacity 1.3 Beta is our new, not quite finished, development version which will be the basis of our next "stable" version.')?></p>

<p><?=_('
 <ul>
  <li>Contains dozens of')?> <a href="features-1.3-a#olderdetails"><?=_('new, exciting features.')?></a> <?=_('Very occasionally, these might need final polishing or not be retained in later versions.')?></li>
  <li><?=_('Occasionally, a feature might not work as it did before, or might be temporarily disabled.</li>
  <li>Some parts of the program are not yet documented or translated into different languages.</li>
 </ul>
')?></p>

<p><?=_('If you are new to Audacity or computer audio programs, you may prefer the fully finished Audacity 1.2. Otherwise, please help our development effort by trying the Beta and <a href="&#x6d;&#x61;&#105;&#108;&#116;&#111;&#x3a;&#97;&#x75;&#x64;&#97;&#99;&#x69;&#x74;&#x79;&#45;&#x66;&#101;&#x65;&#x64;&#x62;&#97;&#x63;&#107;&#x40;&#x6c;&#105;&#115;&#x74;&#115;&#46;&#115;&#111;&#x75;&#x72;&#99;&#101;&#x66;&#x6f;&#114;&#103;&#101;&#46;&#110;&#x65;&#116;&#63;&#x73;&#x75;&#98;&#x6a;&#x65;&#x63;&#x74;&#61;&#x42;&#x65;&#116;&#x61;&#32;&#102;&#101;&#101;&#x64;&#98;&#x61;&#99;&#107;">send us your comments</a>. Please mention your operating system, full details of any problems, and any features that need improvement or no longer work properly. You can install 1.3 and 1.2 on the same machine by installing them to different directories.')?></p>

<p><?=_('<b>Note:</b> In line with our <a href="../contact/privacy">Privacy Policy</a>, feedback you give about the Beta is sent to a public mailing list - messages are seen by the list subscribers, and archived on several web sites.')?></p>

<p><?=$downloadTagline?></p>

<h3 id="recdown"><?=_("Recommended Download")?></h3>
<?php include "recommended.inc.php"?>
<ul>
  <li><p>Universal Binary: <?php printf(_('<a href="%s">Audacity %s</a> (.dmg file, %.1lf MB)'), "../beta/".$macosx_ub_url, macosx_ub_version, macosx_ub_size)?></p></li>
</ul>

<p> <b>Installation instructions:</b>
 <ol>
   <li>Inside your Applications folder, create a folder called "Audacity"</li>
   <li>Double-click the downloaded .dmg to mount it</li>
   <li>Option-drag the whole of the .dmg contents (not the .dmg itself) into the "Audacity" folder you created</li>
   <li>Double-click Audacity.app inside the Applications folder to launch it</li>
 </ol>
</p>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<ul>
  <li><p><a href="http://ardour.org/files/releases/swh-plugins-0.4.15.dmg">LADSPA plugins installer</a> - over 90 plug-ins.</p></li>
<?php include "common.inc.php"; ?>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.3 requires Mac OS X 10.3 or later.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
