<?

# perform a redirect to the latest version, so links to it can remain static

// logging
$d = date("Y-m-d G:i:s");
$fp = fopen("download_stats.txt", "a");
fwrite($fp, "$d\tlinux-i386\n");
fclose($fp);

include 'mirror.php.inc';
include 'versions.php.inc';
header('Location: '.mirror.'audacity-linux-i386-'.linux386.'.tar.bz2');

?>
