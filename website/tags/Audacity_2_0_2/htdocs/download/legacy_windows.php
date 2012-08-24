<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2007 - 12 Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../legacy/versions.inc.php";
  require_once "../legacy/mirror.inc.php";
  $pageId = "windows";
  $pageTitle = _("Legacy Windows");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<div class="advice">
<?=_('These are legacy versions of Audacity for Windows 98/ME/2000/XP.<p><a href="http://wiki.audacityteam.org/wiki/Windows_Vista_OS">Windows Vista</a> <b>and</b> <a href="http://wiki.audacityteam.org/wiki/Windows_7_OS">Windows 7</a> <b>are not supported in any Audacity versions on this page</b>. Please use <a href="/download/windows">the latest Windows version</a> for Vista and 7.</p>')?>
</div>

<h3><?=_("Download")?></h3>
<p>
<a href="#sysreq"><?=_("System Requirements")?></a>
</p>
<h4><?=_("Audacity 2.0.0 for Windows 98/ME")?></h4>
<p>
  <?=_("Version 2.0.0 of Audacity is the final, recommended version for Windows 98/ME.")?>
</p>

<ul>
  <li><p><?=_('<a href="http://audacity.googlecode.com/files/audacity-win-2.0-ansi.exe">Audacity 2.0.0 installer</a> (.exe file, 19.2 MB, including help files) for <b>Windows 98/ME</b>')?></p></li>
  <li><p><?=_('<a href="http://audacity.googlecode.com/files/audacity-win-2.0-ansi.zip">Audacity 2.0.0 zip file</a> (7.8 MB) for <b>Windows 98/ME</b> - Use this if you want a smaller download (without help files), or cannot run the installer because of restricted permissions.')?></p></li>
</ul>

<h4>Audacity 1.2.6</h4>
<p>
  <?=_('Please consider using 2.0.0 above for Windows 98/ME or the <a href="/download/windows">latest 2.0.x version</a> for Windows 2000/XP.')?>
</p>
<ul>
  <li><p><?php printf(_('<a href="%s">Audacity %s installer</a> (.exe file, %.1lf MB) for <b>Windows 98/ME/2000/XP</b>'), download_url($win_exe_url), win_exe_version, win_exe_size)?></p></li>
  <li><p><?php printf(_('<a href="%s">Audacity %s zip file</a> (%.1lf MB) for <b>Windows 98/ME/2000/XP</b> - If you cannot run the installer because of restricted permissions, download and unzip this file instead.'), download_url($win_zip_url), win_zip_version, win_zip_size)?></p></li>
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
  <li><p><?php printf(_('<a href="%s">LADSPA plug-ins %s installer</a> (.exe file, %.1lf MB) - over 90 plug-ins.'), download_url($ladspa_url), ladspa_version, ladspa_size)?></p></li>
  <li><p><a href="plugins"><?=_("Plug-Ins")?></a> - <?=_("Download additional effects and filters.")?></p></li>
  <li><p><a href="../help/faq?s=install&amp;item=lame-mp3"><?=_("LAME MP3 encoder")?></a> - <?=_("Allows Audacity to export MP3 files.")?></p></li>
  <li><p><a href="http://manual.audacityteam.org/index.php?title=FAQ:Installation_and_Plug-Ins#installffmpeg"><?=_("FFmpeg import/export library for Audacity 2.0.0 only")?></a> - <?=_("Allows Audacity to import and export many additional audio formats such as AC3, AMR(NB), M4A and WMA, and to import audio from video files.")?></p></li>
</ul>

<h4><?=_("Alternative Download Links")?></h4>    
<ul>
  <li>
    <p><?php printf(_('If you have trouble with your download, or need an older version of Audacity, try:')) ?></p>
    <ul>
      <li><?php printf(_('<a href="%s">SourceForge</a>: View older versions by clicking to enter the required folder under "Name".'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?></li>
      <li><?php printf(_('<a href="%s">Google Code</a>: Click on the headings to sort the list.'), "http://code.google.com/p/audacity/downloads/list")?></li>
    </ul>
  </li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<p>
  <?=_("Windows 95 and NT are not supported. Windows Vista and 7 are supported in <a href=\"windows\">the latest version of Audacity</a>.")?>
</p>

<p>
  <?=_('The values in the "Recommended RAM/processor speed" column below are for tasks like recording for an hour, or editing three 20-minute tracks simultaneously. The values in the "Minimum RAM/processor speed" column will be fine for smaller/shorter tasks, especially if unnecessary programs are closed.')?>
</p>

<table class="winsysreq">
  <tr>
    <th><?=_("Windows version")?></th>
    <th><?=_("Recommended RAM/<br>processor speed")?></th>
    <th><?=_("Minimum RAM/<br>processor speed")?></th>
  </tr>
  <tr>
    <td>Windows 2000, XP</td>
    <td>512 MB/1 GHz</td>
    <td>128 MB/300 MHz</td>
  </tr>
  <tr>
    <td class="version">Windows 98, ME</td>
    <td class="ramspeed">128 MB / 500 MHz</td>
    <td class="ramspeed">64 MB / 300 MHz</td>
  </tr>
</table>

<p>&nbsp;</p>
<p>
  <?=_("Generally, Audacity works best on computers meeting more than the minimum requirements in the table above. Where Audacity is to be used for lengthy multi-track projects, we recommend using Windows XP, Vista or 7 running on machines of substantially higher specification than the minimum stated above.")?>
</p>


<?php
  include "../include/footer.inc.php";
?>
