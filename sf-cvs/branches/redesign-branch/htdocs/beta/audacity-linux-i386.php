<?

# perform a redirect to the latest version, so links to it can remain static

include 'mirror.php.inc';
include 'versions.php.inc';
header('Location: '.mirror.'audacity-linux-i386-'.linux.'.tar.bz2');

?>
