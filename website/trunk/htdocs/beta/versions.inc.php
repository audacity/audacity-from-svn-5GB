<?php
// The main version number, when we don't distinguish between platforms
define('beta_version', '1.3.12');

// Note: Sizes are in MB.
define('beta_macosx_version', '1.3.12');

define('macosx_ub_version', '1.3.12');
define('macosx_ub_size',    17.1);

define('macosx_ub_zip_version', '1.3.12');
define('macosx_ub_zip_size',    11.0);


//define('macosx_ppc_version', '1.3.3');
//define('macosx_ppc_size',    7.2);

//define('macosx_intel_version', '1.3.3');
//define('macosx_intel_size',    7.2);

//define('macosx_ppc_unicode_version', '1.3.2');
//define('macosx_ppc_unicode_size',    4.2);

//define('macosx_intel_unicode_version', '1.3.2');
//define('macosx_intel_unicode_size',    4.0);



define('beta_win_exe_version', '1.3.12');

define('win_exe_version', '1.3.12');
define('win_exe_size',    10.7);

define('win_zip_version', '1.3.12');
define('win_zip_size',    7.8);

define('win_exe_unicode_version', '1.3.12');
define('win_exe_unicode_size',    11.3);

define('win_zip_unicode_version', '1.3.12');
define('win_zip_unicode_size',    8.1);


define('ladspa_version', '0.4.15');
define('ladspa_size',    1.5);


define('beta_src_version', '1.3.12');
define('src_version', '1.3.12-beta');
define('min_src_size',    5.8);
define('full_src_size',    17.0);
define('src_suffix',  '.tar.bz2');

$win_exe_url = "http://audacity.googlecode.com/files/audacity-win-" .win_exe_version.".exe";
$win_zip_url = "http://audacity.googlecode.com/files/audacity-win-" .win_zip_version.".zip";
$win_exe_unicode_url = "http://audacity.googlecode.com/files/audacity-win-unicode-" .win_exe_unicode_version.".exe";
$win_zip_unicode_url = "http://audacity.googlecode.com/files/audacity-win-unicode-" .win_zip_unicode_version.".zip";

$min_src_url = "audacity-minsrc-".src_version.src_suffix;
$full_src_url = "audacity-fullsrc-".src_version.src_suffix;

$ladspa_url = "ladspa/ladspa-".ladspa_version.".exe";

$macosx_ub_url = "http://audacity.googlecode.com/files/audacity-macosx-ub-" .macosx_ub_version.".dmg";

$macosx_ub_zip_url = "http://audacity.googlecode.com/files/audacity-macosx-ub-" .macosx_ub_zip_version.".zip";

$macosx_intel_url = "audacity-macosx-intel/audacity-macosx-intel-".macosx_intel_version.".dmg";
$macosx_ppc_url = "audacity-macosx-ppc/audacity-macosx-ppc-".macosx_ppc_version.".dmg";
$macosx_intel_unicode_url = "audacity-macosx-intel-unicode/audacity-macosx-intel-unicode-".macosx_intel_unicode_version.".dmg";
$macosx_ppc_unicode_url = "audacity-macosx-ppc-unicode/audacity-macosx-ppc-unicode-".macosx_ppc_unicode_version.".dmg";
?>