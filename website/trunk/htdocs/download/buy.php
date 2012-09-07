<?php
/*
 * Copyright 2004 - 12 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "buy";
  $pageTitle = _("Obtain Audacity on CD");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('Audacity is always free to download from us, but third parties can also sell or otherwise distribute our program, thanks to the <a href="../about/license#license">GNU General Public License</a>. Note that these are often not the latest version of Audacity, so we suggest you check before purchasing.')."</p>";

  echo _('<h3>Windows or Linux </h3>');

  echo "<p><ul>";

  printf (_('<li><a href="%s">OpenDisc</a> is a collection of high quality free and open source software for <b>Windows</b>. You can download the latest CD image for free, or donate to receive a CD copy anywhere in the world.</li>'), "http://theopendisc.com/");
  printf (_('<li><a href="%s">FreedomSampler&trade;</a> is a collection of free, open source software for <b>Windows and Linux</b> with full source code, allowing easy redistribution under the GNU General Public License. ISO images may be downloaded for free, or you can <a href="http://www.twistedlincoln.com/shop/catalog/products/freedomsampler">purchase</a> a copy to be shipped to you (US only)</li>'), "http://www.freedomsampler.com/"); 
  printf (_('<li><a href="%s">VALO-CD</a> contains twenty-two open-source <b>Windows</b> programs on a single CD. You can download and burn the ISO image for free, or purchase the CD for worldwide shipping.</li>'), "http://www.valo-cd.net/");
  printf (_('<li><a href="%s">TheOpenCD</a> is a now-discontinued collection of high quality free and open source software for <b>Windows</b>. Currently, you can still <a href="%s">download archived CD images for free</a>, or <a href="%s">purchase a CD online</a> for worldwide shipping.</li>'), "http://www.theopencd.org/", "http://releases.theopencd.org/", "http://www.thelinuxshop.co.uk/catalog/product_info.php?products_id=189");

  echo "</ul></p>";

  include "../include/footer.inc.php";
?>