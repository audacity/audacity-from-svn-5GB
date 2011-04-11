<?php
/* The main version number, when we don't distinguish between platforms.
 * Under normal circumstances (i.e. all platforms release together), we should
 * only need to update this number and the various sizes for a new beta release.
 * Versions only need coding for specific platforms if they are lagging behind
 * the current release version. */
define('beta_version', '1.3.13');

/* Specific download versions below. Only define these if the version for that
 * download is different to the current beta above. You do need to go
 * through and set the correct sizes for each download however. */
// Note: Sizes are in MB.

// version for all MacOS releases
define('beta_macosx_version', beta_version);
// version and size for Universal Binary DMG
define('macosx_ub_version', beta_macosx_version);
define('macosx_ub_size',    22.5);
// version and size for Universal Binary zip file
define('macosx_ub_zip_version', beta_macosx_version);
define('macosx_ub_zip_size',    13.7);

// version for all Win releases
define('beta_win_version', beta_version);
// version and size for Win ANSI installer
define('win_exe_version', beta_win_version);
define('win_exe_size',    13.2);
// version and size for Win ANSI zip
define('win_zip_version', beta_win_version);
define('win_zip_size',    7.9);
// version and size for Win Unicode installer
define('win_exe_unicode_version', beta_win_version);
define('win_exe_unicode_size',    13.8);
// version and size for Win Unicode zip
define('win_zip_unicode_version', beta_win_version);
define('win_zip_unicode_size',    7.6);

// version for source code releases (should always track beta_version)
define('src_version', beta_version);
// sizes of minimal and full source tarballs
define('min_src_size',    6.2);
define('full_src_size',    17.6);
// suffix of tarballs (in case we change compression etc)
define('src_suffix',  '.tar.bz2');

// LADSPA plugins for Windows installer, separate version series
define('ladspa_version', '0.4.15');
define('ladspa_size',    1.5);

/* from here on, build up variables with the URLs in them. These shouldn't need
 * to be edited when new releases are made.
 * The leading part of URLs is not stored here but rather in mirror.inc.php 
 * which provides the download_url() function to which these variables should
 * be passed as the sole argument */
$win_exe_url = "audacity-win-" .win_exe_version.".exe";
$win_zip_url = "audacity-win-" .win_zip_version.".zip";
$win_exe_unicode_url = "audacity-win-unicode-" .win_exe_unicode_version.".exe";
$win_zip_unicode_url = "audacity-win-unicode-" .win_zip_unicode_version.".zip";

$min_src_url = "audacity-minsrc-".src_version."-beta".src_suffix;
$full_src_url = "audacity-fullsrc-".src_version."-beta".src_suffix;

$ladspa_url = "LADSPA_plugins-win-".ladspa_version.".exe";

$macosx_ub_url = "audacity-macosx-ub-" .macosx_ub_version.".dmg";

$macosx_ub_zip_url = "audacity-macosx-ub-" .macosx_ub_zip_version.".zip";

$macosx_intel_url = "audacity-macosx-intel/audacity-macosx-intel-".macosx_intel_version.".dmg";
$macosx_ppc_url = "audacity-macosx-ppc/audacity-macosx-ppc-".macosx_ppc_version.".dmg";
$macosx_intel_unicode_url = "audacity-macosx-intel-unicode/audacity-macosx-intel-unicode-".macosx_intel_unicode_version.".dmg";
$macosx_ppc_unicode_url = "audacity-macosx-ppc-unicode/audacity-macosx-ppc-unicode-".macosx_ppc_unicode_version.".dmg";
?>