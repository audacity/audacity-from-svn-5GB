<?php
/*
 * Copyright 2004 - 2013
 * Matt Brubeck
 * Dominic Mazzoni
 * Richard Ash
 * Gale Andrews 
 * Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  require_once "../latest/mirror.inc.php";
  $pageId = "mac";
  $pageTitle = _("Mac OS X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Downloads - Latest Version of Audacity")?></h3>
<ul>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s</a> (.dmg file, %.1lf MB, including help files) for <b>OS X 10.4 or later</b> (Universal Binary)'), 
                    download_url($macosx_url), macosx_version, macosx_size)?>
    </p>
  </li>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s zip file</a> (%.1lf MB) for <b>OS X 10.4 or later</b> (Universal Binary) - Use this if you want a smaller download (without help files).'),
                    download_url($macosx_zip_url), macosx_version, macosx_zip_size)?>
    </p>
  </li>
</ul>

<p>
  &nbsp;&nbsp;&nbsp;<a href="#sysreq"><?=_("System Requirements")?></a>
</p>

<p> <b><?=_("Installation instructions (.dmg files)")?>:</b>
  <ol>
   <li><?=_("Double-click the downloaded .dmg to mount it.")?></li>
   <li><?=_("Copy the \"Audacity\" folder from the newly mounted .dmg to <b>/Applications</b> (or any other location of your choosing).")?></li>
   <li><?=_("Eject the .dmg at bottom left of Finder, then launch Audacity.app from the \"Audacity\" folder that you copied.")?></li> 
 </ol>
</p>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<h4><?=_("Plug-ins and Libraries")?></h4>
<ul>
  <li><p><a href="http://audacity.googlecode.com/files/swh-plugins-mac-0.4.15.zip"><?=_("LADSPA plug-ins zip file</a> - over 90 plug-ins.")?></p></li>
  <li>
    <p>
      <a href="plugins">
        <?=_("Plug-Ins")?>
      </a> - <?=_("Download additional effects and filters.")?>
    </p>
  </li>
  <li>
    <p>
      <a href="http://manual.audacityteam.org/help/manual/man/faq_installation_and_plug_ins.html#maclame">
        <?=_("LAME MP3 encoder")?>
      </a> - <?=_("Allows Audacity to export MP3 files.")?>
    </p>
  </li>
  <li>
    <p>
      <a href="http://manual.audacityteam.org/o/man/faq_installation_and_plug_ins.html#ffdown">
        <?=_("FFmpeg import/export library")?>
      </a> - <?=_("Allows Audacity to import and export many additional audio formats such as AC3, AMR(NB), M4A and WMA, and to import audio from video files.")?>
    </p>
  </li>
</ul>
<h4>
  <?=_("Alternative Download Links")?>
</h4>
<ul>
  <li>
    <p>
      <?php printf(_('If you have trouble with your download, or need an older version of Audacity, try:')) ?>
    </p>
    <ul>
      <li>
        <?php printf(_('<a href="%s">SourceForge</a>: View older versions by clicking to enter the required folder under "Name".'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?>
      </li>
      <li>
        <?php printf(_('<a href="%s">Google Code</a>: Click on the headings to sort the list.'), "http://code.google.com/p/audacity/downloads/list")?>
      </li>
    </ul>
  </li>
</ul>

<h4>
  <?=_("Audacity Nightly Builds")?>
</h4>
<ul>
  <li><p><?php echo _('<b>For advanced users</b>, <a href="http://wiki.audacityteam.org/index.php?title=Nightly_Builds#mac">Nightly Builds</a> for <b>OS X 10.4 or later</b> (Universal Binary) are available for testing purposes.')?>
    <?php include "beta_nightly.inc.php"; ?></p></li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<p>
<div class="advice">
<b><?=_("Audacity runs best with at least 1 GB RAM and a 1 GHz processor (2 GB RAM/2 GHz on OS X 10.7 or later).")?></b> 
<p><?=_("Where Audacity is to be used for lengthy multi-track projects, we recommend a minimum of 2 GB RAM and 2 GHz processor (4 GB RAM on OS X 10.7 or later).")?></p>
</div>
</p>

<?php
  include "../include/footer.inc.php";
?>
