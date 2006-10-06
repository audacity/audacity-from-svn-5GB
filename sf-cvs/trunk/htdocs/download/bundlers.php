<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "bundlers";
  $pageTitle = _("Vendors that Bundle Audacity");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('Audacity is free to download, but vendors are also free to bundle copies of the program with their products, thanks to the <a href="../about/license">GNU General Public License</a>. The following are legitimate, legal offerings. We are happy and proud that they chose to bundle Audacity with their products.')."</p>";

  echo _('<h3>M-Audio and Pinnacle (Avid) Podcast Factory&trade;</h3>
  <p><a href="http://www.m-audio.com/products/en_us/PodcastFactory-main.html">M-Audio</a> and 
<a href="http://www.pinnaclesys.com/PublicSite/uk/Products/Consumer+Products/Audio/M-Audio/Family?code=UKMaudio">Pinnacle</a> Podcast Factory&trade; is an all-in-one hardware/software solution that makes it easy to record, edit and distribute professional-sounding podcasts. It comes with a broadcast-quality microphone and professional 24-bit/48kHz USB audio interface for making top-notch voice and voice+music recordings. Use the included software (Audacity) to edit your recordings, create your podcast and publish it to the Web.</p>');

  include "../include/footer.inc.php";
?>