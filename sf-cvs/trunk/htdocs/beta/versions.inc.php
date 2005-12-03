<?php
// The main version number, when we don't distinguish between platforms
define('beta_version', '1.3.0');

// Note: Sizes are in MB.
define('beta_macosx_version', '1.3.0b');
define('macosx_version', '1.3.0b');
define('macosx_size',    4.0);

define('beta_win_exe_version', '1.3.0b');
define('win_exe_version', '1.3.0b');
define('win_exe_size',    2.5);

define('win_zip_version', '1.3.0b');
define('win_zip_size',    3.6);

define('beta_src_version', '1.3.0b');
define('src_version', '1.3.0b');
define('src_size',    4.7);
define('src_suffix',  '.tar.gz');

$win_exe_url = "audacity-win/audacity-win-".win_exe_version.".exe";
$win_zip_url = "audacity-win-zip/audacity-win-".win_zip_version.".zip";
$src_url = "audacity-src/audacity-src-".src_version.src_suffix;
$macosx_url = "audacity-macosx/audacity-macosx-".macosx_version.".dmg";
?>
