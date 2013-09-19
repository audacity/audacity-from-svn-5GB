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
  $pageId = "windows";
  $pageTitle = _("Windows");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Downloads - Latest Version of Audacity")?></h3>
<ul>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB, including help files) for <b>Windows&nbsp;2000/XP/Vista/Windows&nbsp;7/Windows&nbsp;8</b>'), 
                    download_url($win_exe_url), win_exe_version, win_exe_size)?>
    </p>
  </li>
  <li>
    <p>
      <?php printf(_('<a href="%s">Audacity %s zip file</a> (%.1lf MB) for <b>Windows&nbsp;2000/XP/Vista/Windows&nbsp;7/Windows&nbsp;8</b> - Use this if you want a smaller download (without help files), or cannot run the installer because of restricted permissions.'),
                    download_url($win_zip_url), win_zip_version, win_zip_size)?>
    </p>
  </li>
</ul>
<p>
  <b>Known Issues:</b>
  After release of 2.0.4, we have had reports that in some rare cases Audacity 2.0.4 has
  <a href="http://forum.audacityteam.org/viewtopic.php?f=46&t=74438">
    issues starting up on Windows</a>.
  We're working on these problems.
  To report or get help with these issues, please
  <a href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#102;&#101;&#101;&#100;&#x62;&#97;&#99;&#107;&#x40;&#97;&#x75;&#x64;&#97;&#x63;&#105;&#116;&#x79;&#116;&#101;&#97;&#x6d;&#46;&#111;&#114;&#x67;">
    email us</a>.
</p>
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
  <li>
    <p>
      <?php echo _('<b>For advanced users</b>, <a href="http://wiki.audacityteam.org/index.php?title=Nightly_Builds#Windows_Binaries">Nightly Builds</a> for <b>Windows 2000/XP/Vista/Windows 7/Windows 8</b> are available for testing purposes.')?>
      <?php include "beta_nightly.inc.php"; ?>
    </p>
  </li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<p>
  <?php printf(_('Read about Audacity on <a href="%s">Windows Vista</a>, <a href="%s">Windows 7</a> and <a href="%s">Windows 8</a>.'), "http://wiki.audacityteam.org/wiki/Windows_Vista_OS", "http://wiki.audacityteam.org/wiki/Windows_7_OS", "http://wiki.audacityteam.org/wiki/Windows_8_OS")?>
  <?=_("Windows 95 and NT are not supported.")?>
</p>

<?php include "windows_sys_reqs.inc.php" ?>

<?php
  include "../include/footer.inc.php";
?>
