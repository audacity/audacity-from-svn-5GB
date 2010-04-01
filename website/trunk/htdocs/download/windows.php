<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2007 - 10 onwards Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  $pageId = "windows";
  $pageTitle = _("Windows");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Download")?></h3>
<a href="#sysreq">System Requirements</a>
<?php include "recommended.inc.php"?>
<ul>
  <li><p><?php printf(_('Windows 98/ME/2000/XP/Vista: <a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB) - The latest version of the free Audacity audio editor. <b>See further information about</b> <a href="http://audacityteam.org/wiki/index.php?title=Windows_Vista_OS">Vista</a><b>.</b>'), "../latest/".$win_exe_url, win_exe_version, win_exe_size)?></p></li>
</ul>

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
  <li><p><?php printf(_('<a href="%s">LADSPA plug-ins %s installer</a> (.exe file, %.1lf MB) - over 90 plug-ins.'), "".$ladspa_url, ladspa_version, ladspa_size)?></p></li>
  <?php include "common.inc.php"; ?>

<h4><?=_("Audacity Zip file")?></h4>
<ul>
  <li><p><?php printf(_('Windows 98/ME/2000/XP/Vista: <a href="%s">Audacity %s zip file</a> (%.1lf MB)
  - If you cannot run the installer because of restricted permissions, download and unzip this file instead.'),
    "../latest/".$win_zip_url, win_zip_version, win_zip_size)?></p></li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<p>
  <?=_("Windows 95 and NT are not supported. Windows 7 is <a href=\"http://wiki.audacityteam.org/wiki/Windows_7_OS\">provisionally supported</a> only in <a href=\"../download/beta_windows\">Audacity Beta</a>.")?>
</p>
<?php include "windows_sys_reqs.inc.php" ?>

<?php
  include "../include/footer.inc.php";
?>
