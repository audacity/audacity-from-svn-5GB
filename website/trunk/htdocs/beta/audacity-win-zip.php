<?

# perform a redirect to the latest version, so links to it can remain static

include 'mirror.inc.php';
include 'versions.inc.php';
// ANSI zip
header('Location: '.download_url($win_zip_url));

?>
