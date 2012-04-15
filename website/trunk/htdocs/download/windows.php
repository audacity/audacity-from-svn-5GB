<?php
/*
 * Copyright 2004 - 2012
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
  $pageId = "windows";
  $pageTitle = _("Windows");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Downloads - Latest Version of Audacity")?></h3>
<ul>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB, including help files) for Windows 2000/XP/Vista/7'), 
                    download_url($win_exe_url), win_exe_version, win_exe_size)?>
    </p>
  </li>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s zip file</a> (%.1lf MB) for Windows 2000/XP/Vista/7 - Use this if you want a smaller download (without help files), or cannot run the installer because of restricted permissions.'),
                    download_url($win_zip_url), win_zip_version, win_zip_size)?>
    </p>
  </li>
</ul>
<p>
  &nbsp;&nbsp;&nbsp;<a href="#sysreq"><?=_("System Requirements")?></a>
</p>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<?php
  // i18n-hint:  If this string is translated, it will appear above the list of
  // optional downloads on the "downloads/windows" page.  If there are extra
  // files available in your language, add them here.  Otherwise, do not
  // translate this string.  (Or make the translation identical to the
  // original string.)
  if (_("localized_downloads_windows") != "localized_downloads_windows") {
    echo _("localized_downloads_windows");
  }
?>
<h4><?=_("Plug-ins and Libraries")?></h4>
<ul>
  <li><p><?php printf(_('<a href="%s">LADSPA plug-ins %s installer</a> (.exe file, %.1lf MB) - Contains over 90 plug-ins.'), download_url($ladspa_url), ladspa_version, ladspa_size)?></p></li>
  <li>
    <p>
      <a href="plugins">
        <?=_("Plug-Ins")?>
      </a> - <?=_("Download additional effects and filters.")?>
    </p>
  </li>
  <li>
    <p>
      <a href="http://manual.audacityteam.org/help/manual/man/faq_installation_and_plug_ins.html#lame">
        <?=_("LAME MP3 encoder")?>
      </a> - <?=_("Allows Audacity to export MP3 files.")?>
    </p>
  </li>
  <li>
    <p>
      <a href="http://manual.audacityteam.org/index.php?title=FAQ:Installation_and_Plug-Ins#installffmpeg">
        <?=_("FFmpeg import/export library")?>
      </a> - <?=_("Allows Audacity to import and export many additional audio formats such as AC3, AMR(NB), M4A and WMA, and to import audio from video files.")?>
    </p>
  </li>
</ul>

<h4>
  <?=_("Audacity for Windows 98/ME")?>
</h4>
<p>
  <?=_("Version 2.0.0 of Audacity is the last that will support Windows 98/ME.")?>
</p>
<ul>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB) for Windows 98/ME'), 
                  download_url($win_ansi_exe_url), win_exe_version, win_exe_size)?>
    </p>
  </li>
  <li>
    </p>
    <?php printf(_('<a href="%s">Audacity %s zip file</a> (%.1lf MB) for Windows 98/ME - If you cannot run the installer because of restricted permissions, download and unzip this file instead.'), 
                  download_url($win_ansi_zip_url), win_zip_version, win_zip_size)?>
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
        <?php printf(_('<a href="%s">SourceForge</a>: View older versions by clicking to enter the required folder under "Name."'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?>
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
  <li>
    <p>
      <?php echo _('<b>For advanced users</b>, <a href="http://wiki.audacityteam.org/index.php?title=Nightly_Builds#Windows_Binaries">Nightly Builds</a> for Windows 2000/XP/Vista/7 are available for testing purposes.')?>
      <?php include "beta_nightly.inc.php"; ?>
    </p>
  </li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<p>
  <?=_("Read about Audacity on Windows <a href=\"http://wiki.audacityteam.org/wiki/Windows_Vista_OS\">Vista</a> and <a href=\"http://wiki.audacityteam.org/wiki/Windows_7_OS\">7</a>.")?>
  <?=_("Windows 95 and NT are not supported.")?>
</p>
<?php include "windows_sys_reqs.inc.php" ?>

<?php
  include "../include/footer.inc.php";
?>
