<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "buy";
  $pageTitle = _("Purchase Audacity on CD");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('Audacity is free to download, but vendors are also free to sell copies of the program, thanks to the <a href="../about/license">GNU General Public License</a>.  If you want a copy of Audacity on CD-ROM, we encourage you to buy from a vendor listed below.')."</p>";

  echo _('<h3>TheOpenCD (Windows)</h3>
  <p><a href="http://www.theopencd.org/">TheOpenCD</a> is a collection of high-quality free software for Microsoft Windows, including Audacity.  You can <a href="http://theopencd.sunsite.dk/wheretobuy.php">purchase a copy of TheOpenCD</a> from several different vendors.</p>');

  include "../include/footer.inc.php";
?>
