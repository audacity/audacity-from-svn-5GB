<?php

header("Content-type: text/html; charset=$charset");

?>

<html>

<head>

  <title><?php print $title; ?></title>

<?php
 print "  <meta name=description content=\"$metaDescription\">\n";
?>

</head>

<body bgcolor="#ffffff">

<!-- main table - nav bar goes on left, top bar and news goes on right -->
<table border="0" cellpadding="0" cellspacing="10" width="100%">
<tr><td valign="top" width="192">

<?php include 'sidebar.php' ?>

</td>

<td valign="top">

<?php include 'topbar.php' ?>

