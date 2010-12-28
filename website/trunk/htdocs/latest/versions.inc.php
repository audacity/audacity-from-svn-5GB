<?php
// The main version number, when we don't distinguish between platforms
define('stable_version', '1.2.6');

// Note: Sizes are in MB.
define('mac_classic_version', '1.0.0');
define('mac_classic_size',    1.5);

define('macosx_version', '1.2.6a');
define('macosx_size',    3.9);
define('macosx_intel_version', '1.2.5');
define('macosx_intel_size',    3.4);

define('win_exe_version', '1.2.6');
define('win_exe_size',    2.1);
define('win_zip_version', '1.2.6');
define('win_zip_size',    3.0);

define('ladspa_version', '0.4.15');
define('ladspa_size',    1.5);

define('src_version', '1.2.6');
define('src_size',    4.6);
define('src_suffix',  '.tar.gz');

/* from here on, build up variables with the URLs in them. These shouldn't need
 * to be edited when new releases are made.
 * The leading part of URLs is not stored here but rather in mirror.inc.php 
 * which provides the download_url() function to which these variables should
 * be passed as the sole argument */
$win_exe_url = "audacity-win-" .win_exe_version.".exe";
$win_zip_url = "audacity-win-" .win_zip_version.".zip";
//$win_exe_unicode_url = "audacity-win-unicode-" .win_exe_unicode_version.".exe";
//$win_zip_unicode_url = "audacity-win-unicode-" .win_zip_unicode_version.".zip";

$macosx_url = "audacity-macosx-ppc-" .macosx_version.".dmg";
$macosx_intel_url = "audacity-macosx-intel-" .macosx_intel_version.".dmg";
$mac_classic_url = "audacity-mac-" .mac_classic_version.".sit";
//$macosx_ub_url = "audacity-macosx-ub-" .macosx_ub_version.".dmg";
//$macosx_ub_zip_url = "audacity-macosx-ub-" .macosx_ub_zip_version.".zip";

$src_url = "audacity-src-" .src_version.src_suffix;
$ladspa_url = "LADSPA_plugins-win-".ladspa_version.".exe";
?>
