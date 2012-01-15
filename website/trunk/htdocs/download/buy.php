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
  echo "<p>"._('Audacity is always free to download from us, but third parties can also sell or otherwise distribute our program, thanks to the <a href="../about/license#license">GNU General Public License</a>. If you want a copy of Audacity on CD-ROM, we encourage you to use the options listed below.')."</p>";

  echo _('<h3>Windows or Linux </h3>
  <p>
   <ul>
    <li><a href="http://www.freedomsampler.com/">FreedomSampler&trade;</a> is a collection of free, open source software for <b>Windows and Linux</b> with full source code, allowing easy redistribution under the GNU General Public License. ISO images may be downloaded for free, or you can <a href="http://www.twistedlincoln.com/shop/catalog/products/freedomsampler">purchase</a> a copy to be shipped to you (US only).</li>
    <li><a href="http://www.theopencd.org/">TheOpenCD</a> is a now-discontinued collection of high quality free and open source software for <b>Windows</b>. Currently, you can still <a href="http://releases.theopencd.org/">download archived CD images</a> or <a href="http://www.thelinuxshop.co.uk/catalog/product_info.php?products_id=189">purchase a CD online</a> for worldwide shipping.</li>
    <li><a href="http://theopendisc.com/">OpenDisc</a> is a similar collection of high quality free and open source software for <b>Windows</b>. Currently, a downloadable <a href="http://downloads.sourceforge.net/opendisc/OpenDisc-07.10.iso?use_mirror=osdn">CD image</a> is available. The project hopes to make available a purchasable CD shortly.</li>
   </ul>
  </p>');

  include "../include/footer.inc.php";
?>