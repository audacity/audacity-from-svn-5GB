<?php

require_once 'main.inc.php';
$title = $releaseNotesStr;
include 'top.inc.php';

BoxTop("$betaVersionsStr");
include GetTranslation("betanotes-1.2.2-pre1");
BoxBottom();
print "<p>\n";

include GetTranslation("betanotes");
include 'bottom.inc.php';

?>

