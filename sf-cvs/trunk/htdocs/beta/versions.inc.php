<?php
// The main version number, when we don't distinguish between platforms
define('beta_version', '1.3.0');

// Note: Sizes are in MB.
define('beta_macosx_version', '1.3.2');

define('macosx_ppc_version', '1.3.2');
define('macosx_ppc_size',    4.2);

define('macosx_intel_version', '1.3.2');
define('macosx_intel_size',    4.1);

define('macosx_ppc_unicode_version', '1.3.2');
define('macosx_ppc_unicode_size',    4.2);

//define('macosx_intel_unicode_version', '1.3.2');
//define('macosx_intel_unicode_size',    4.0);



define('beta_win_exe_version', '1.3.2');

define('win_exe_version', '1.3.2');
define('win_exe_size',    2.7);

define('win_zip_version', '1.3.2');
define('win_zip_size',    3.6);

define('win_exe_unicode_version', '1.3.2');
define('win_exe_unicode_size',    2.7);

define('win_zip_unicode_version', '1.3.2');
define('win_zip_unicode_size',    3.6);


define('ladspa_version', '0.4.15');
define('ladspa_size',    1.5);


define('beta_src_version', '1.3.2');
define('src_version', '1.3.2');
define('src_size',    6.4);
define('src_suffix',  '.tar.gz');

$win_exe_url = "audacity-win/audacity-win-".win_exe_version.".exe";
$win_zip_url = "audacity-win-zip/audacity-win-".win_zip_version.".zip";
$win_exe_unicode_url = "audacity-win-unicode/audacity-win-unicode-".win_exe_unicode_version.".exe";
$win_zip_unicode_url = "audacity-win-unicode-zip/audacity-win-unicode-".win_zip_unicode_version.".zip";

$src_url = "audacity-src/audacity-src-".src_version.src_suffix;

$ladspa_url = "ladspa/ladspa-".ladspa_version.".exe";

$macosx_intel_url = "audacity-macosx-intel/audacity-macosx-intel-".macosx_intel_version.".dmg";
$macosx_ppc_url = "audacity-macosx-ppc/audacity-macosx-ppc-".macosx_ppc_version.".dmg";
$macosx_intel_unicode_url = "audacity-macosx-intel-unicode/audacity-macosx-intel-unicode-".macosx_intel_unicode_version.".dmg";
$macosx_ppc_unicode_url = "audacity-macosx-ppc-unicode/audacity-macosx-ppc-unicode-".macosx_ppc_unicode_version.".dmg";
?>
