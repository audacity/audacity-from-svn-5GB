<?php

require_once 'main.inc.php';
$title = $releaseNotesStr;
include 'top.inc.php';

BoxTop("$betaVersionsStr");
include GetTranslation("betanotes-1.2.0");
include GetTranslation("betanotes-1.2.0-pre4");
include GetTranslation("betanotes-1.2.0-pre1");
BoxBottom();
print "<p>\n";

include GetTranslation("betanotes");
include 'bottom.inc.php';

?>

