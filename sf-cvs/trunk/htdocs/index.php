<?php

require_once 'main.inc.php';
$title = "Audacity";
include 'top.inc.php';

$n = 1;
while(is_file("en/news$n.php"))
	$n++;

for($i=$n-1; $i>=1; $i--) {
	include GetTranslation("news$i");
   print "<p>\n";
}

include 'bottom.inc.php';

?>






