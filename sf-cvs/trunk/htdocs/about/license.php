<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Dominic Mazzoni, Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "license";
  $pageTitle = _("License, and Advice for Vendors");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php

// i18n-hint:  These gnu.org web pages are available in several languages.
// Please link to the version in your language, if possible.  For example:
//
//   http://www.gnu.org/licenses/licenses.es.html#GPL
//   http://www.gnu.org/licenses/gpl-faq.cs.html
//
// See the bottom of each page for versions in other languages.
//
echo _('<h3>License</h3>

<p>Audacity is free software; you can redistribute it and/or modify it under the terms of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GNU General Public License</a> as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.</p>

<p>This license gives you the permission to modify, copy, distribute, and sell Audacity, as long as the code you distribute is available under the GPL.  If you have questions, please <a href="../contact/">contact us</a>.

<p>See also: <a href="http://www.gnu.org/licenses/gpl-faq.html">Frequently Asked Questions about the GNU GPL</a>.</p>');


echo _('<h3>Advice for Vendors</h3>

<p>Several vendors bundle Audacity with their products, resell, or distribute Audacity (see <a href="../download/bundlers">Vendors that Distribute Audacity</a>). Here are guidelines for bundling or reselling Audacity.</p>

<p>
  <h4>For vendors selling a hardware product, with Audacity included:</h4>
  <ol>
    <li>Thank you for choosing to distribute Audacity with your product! Please contact us early so that we can work together to help make your product a success.</li>
    <li>Please distribute Audacity unmodified. If you need to modify it to work with your device, please contact us first.</li>
    <li>Please test your device with Audacity thoroughly.  If there are any specific configuration instructions that users need to follow in order for your device to work with Audacity, make sure they are included with your device.</li>
    <li>Please do not refer customers to Audacity tech support when they are having difficulties using your device.  If you are selling a commercial product, you should provide your own tech support.  When users are having Audacity-specific issues that are not related to your specific device, we will of course be happy to assist them.</li>
    <li>If you modify Audacity without the express permission of the Audacity team, you do not have the right to use the name Audacity, which is registered trademark of Dominic Mazzoni.  You must rename the product and provide all of your own tech support.</li>
  </ol>
</p>

<p>
  <h4>For vendors who wish to resell or distribute Audacity:</h4>
  <ol>
    <li>If you are distributing Audacity unmodified, your advertising should make it clear that Audacity is available for free on the Internet, and make it clear what the customer is paying for when they purchase Audacity from you.  Possiblilties include the convenience of Audacity on CD-ROM, other bundled software or music files, a printed manual, or more.</li>
    <li>You may not advertise your business as an authorized distributor of Audacity.  The name Audacity is a registered trademark of Dominic Mazzoni.</li>
    <li>If you modify Audacity without the express permission of the Audacity team, you do not have the right to use the name Audacity, which is registered trademark of Dominic Mazzoni.  You must rename the product and provide all of your own tech support.</li>
  </ol>
</p>');
?>

<!-- TODO: Copyright and licenses for libraries. -->

<?php
  include "../include/footer.inc.php";
?>
