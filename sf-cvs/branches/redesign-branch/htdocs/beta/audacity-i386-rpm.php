<?

# perform a redirect to the latest version, so links to it can remain static

include 'mirror.php.inc';
include 'versions.php.inc';
header('Location: '.mirror.'audacity-'.i386rpm.'.i386.rpm');

?>
