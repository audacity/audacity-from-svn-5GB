<?php

require_once 'main.inc.php';
$title = $releaseNotesStr;
include 'top.inc.php';

php BoxTop("$releaseNotesStr 1.2.0-pre1");
include GetTranslation("betanotes-1.2.0-pre1");
BoxBottom();

include GetTranslation("betanotes");
include 'bottom.inc.php';

?>

