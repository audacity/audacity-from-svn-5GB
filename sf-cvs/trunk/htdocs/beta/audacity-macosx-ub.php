<?

# perform a redirect to the latest version, so links to it can remain static

include 'mirror.inc.php';
include 'versions.inc.php';
header('Location: '.download_url('audacity-macosx-ub-'.macosx_ub_version.'.dmg'));

?>
