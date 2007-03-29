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
  include "../include/header_adsense.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('Audacity is free to download, but vendors are also free to bundle copies of the program with their products, thanks to the <a href="../about/license">GNU General Public License</a>. The following are legitimate, legal offerings. We are happy and proud that they chose to bundle Audacity with their products.')."</p>";

  echo _('<h3>Behringer iAXE393 USB-Guitar</h3>
  <p>The <a href="http://www.behringer.com/IAXE393/">Behringer iAXE393 USB-Guitar</a> 
is an electric guitar that connects to the computer via USB and has headphones output. It comes with the "Guitar Combos BEHRINGER Edition" from Native Instruments, software emulations of three amp/effects combos.</p>');

  echo _('<h3>Ion&reg; iTTUSB and Numark&reg; ttUSB turntables</h3>
  <p>The <a href="http://www.ion-audio.com/ittusb.php">Ion iTTUSB</a> and 
<a href="http://www.numark.com/products/product_view.php?v=overview&n=144">Numark ttUSB</a> turntables allow users to record vinyl and shellac records into Audacity via the USB port on the computer, so neither a conventional turntable nor line-in is required.</p>
   <p>The Ion iTTUSB also provides a line-in port where an external device such as a cassette player can be attached and recorded into Audacity through the USB port. The Numark ttUSB is similar, made by a sister company, and adds a variable pitch control.</p>
   <p>Currently these turntables disable system sound on Windows machines until they are unplugged at the USB connection. This is correctable by resetting the system playback device in the Windows mixer. They also have no volume control nor apparently any ability to vary the input level of the recording.</p>');

  echo _('<h3>Podcast Factory&trade; from M-Audio (U.S.) and Pinnacle&trade; (Europe) divisions of Avid</h3>
  <p>Podcast Factory from <a href="http://www.m-audio.com/products/en_us/PodcastFactory-main.html">M-Audio</a> and 
<a href="http://www.pinnaclesys.com/PublicSite/uk/Products/Consumer+Products/Audio/M-Audio/Family?code=UKMaudio">Pinnacle</a> is an all-in-one hardware/software solution that makes it easy to record, edit and distribute professional-sounding podcasts. It comes with a broadcast-quality microphone and professional 24-bit/48kHz USB audio interface for making top-notch voice and voice+music recordings. Use the included Audacity software to edit your recordings, create your podcast, and publish it to the Web.</p>');

  include "../include/footer.inc.php";
?>