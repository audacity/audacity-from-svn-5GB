<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  $pageId = "windows";
  $pageTitle = _("Windows");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Download")?></h3>
<ul>
  <li><p><?php printf(_('<a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB) - The latest version of the free Audacity audio editor.'), "../latest/".$win_exe_url, win_exe_version, win_exe_size)?></p></li>
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
<ul>
  <?php include "common.inc.php"; ?>
  <li><p><?php printf('<a href="%s">Audacity %s zip file</a> (%.1lf MB)
  - If you can’t run the installer because you are in a computer lab or other restricted environment, download and unzip this file instead.',
    "../latest/".$win_zip_url, win_zip_version, win_zip_size)?></p></li>
</ul>

<h3><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Windows 98, ME, 2000, XP, or later.  (Sorry, Audacity is not supported on Windows 95 or NT 4.0.)")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
