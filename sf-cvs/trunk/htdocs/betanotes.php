<?php

require_once 'main.inc.php';
$title = $releaseNotesStr;
include 'top.inc.php';

BoxTop("$releaseNotesStr 1.2.0-pre4");
include GetTranslation("betanotes-1.2.0-pre4");
include GetTranslation("betanotes-1.2.0-pre1");
BoxBottom();
print "<p>\n";

include GetTranslation("betanotes");
include 'bottom.inc.php';

?>

