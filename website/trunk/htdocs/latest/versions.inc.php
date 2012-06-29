<?php
/* The main version number, when we don't distinguish between platforms.
 * Under normal circumstances (i.e. all platforms release together), we should
 * only need to update this number and the various sizes for a new beta release.
 * Versions only need coding for specific platforms if they are lagging behind
 * the current release version. */
define('stable_version', '2.0.0');

/* Specific download versions below. Only define these if the version for that
 * download is different to the value above. You do need to go
 * through and set the correct sizes for each download however. */
// Note: Sizes are in MB.
define('macosx_version', '2.0.0');
define('macosx_size',    27.5);
define('macosx_zip_size',    12.8);

define('win_exe_version', '2.0');
define('win_exe_size',    19.8);
define('win_zip_version', '2.0');
define('win_zip_size',    7.7);


define('ladspa_version', '0.4.15');
define('ladspa_size',    1.5);

define('src_version', '2.0.0');
define('full_src_size',    17.9);
define('min_src_size',    6.3);
define('src_suffix',  '.tar.bz2');

/* from here on, build up variables with the URLs in them. These shouldn't need
 * to be edited when new releases are made.
 * The leading part of URLs is not stored here but rather in mirror.inc.php 
 * which provides the download_url() function to which these variables should
 * be passed as the sole argument */
$win_exe_url = "audacity-win-" .win_exe_version.".exe";
$win_zip_url = "audacity-win-" .win_zip_version.".zip";
$win_ansi_exe_url = "audacity-win-" .win_exe_version."-ansi.exe";
$win_ansi_zip_url = "audacity-win-" .win_zip_version."-ansi.zip";

$macosx_url = "audacity-macosx-ub-" .macosx_version.".dmg";
$macosx_zip_url = "audacity-macosx-ub-" .macosx_version.".zip";

$min_src_url = "audacity-minsrc-".src_version.src_suffix;
$full_src_url = "audacity-fullsrc-".src_version.src_suffix;

$ladspa_url = "LADSPA_plugins-win-".ladspa_version.".exe";
?>
