<?php

require_once 'main.inc.php';
$title = $releaseNotesStr;
include 'top.inc.php';

?>
<?php BoxTop("$releaseNotesStr $stableVersion"); ?>
<?php include GetTranslation("beta-1.2.0-pre3-problems"); ?>
<?php include GetTranslation("1.2.3-changes"); ?>
<?php include GetTranslation("1.2.2-changes"); ?>
<?php include GetTranslation("1.2.1-changes"); ?>
<?php include GetTranslation("beta-1.2.0-pre1-changes"); ?>
<?php BoxBottom(); ?>

<?php
include 'bottom.inc.php';
?>
