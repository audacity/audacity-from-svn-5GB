<?

# perform a redirect to the latest version, so links to it can remain static

// logging
$d = date("Y-m-d G:i:s");
$fp = fopen("download_stats.txt", "a");
fwrite($fp, "$d\twin-zip\n");
fclose($fp);

include 'mirror.inc.php';
include 'versions.inc.php';
header('Location: '.download_url('audacity-win-'.win_zip_version.'.zip'));

?>
