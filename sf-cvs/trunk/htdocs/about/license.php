<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Dominic Mazzoni, Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "license";
  $pageTitle = _("License, and Advice for Vendors and Distributors");
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

<p>Audacity is free software; you can use it for any personal, commercial, or educational purpose, including installing it on as many different computers as you wish.</p>

<p>You can also copy, redistribute, modify, or resell it and/or modify it under the terms of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GNU General Public License (GPL)</a> as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. In granting you this right, the GPL requires that the source code you distribute is itself available under the same GPL. If you have questions, please <a href="../contact/">contact us</a>.</p>

<p>See also: <a href="http://www.gnu.org/licenses/gpl-faq.html">Frequently Asked Questions about the GNU GPL</a>.</p>');


echo _('<h3>Advice for Vendors and Distributors</h3>

<p>Several third parties bundle Audacity with their products, resell, or distribute Audacity (see <a href="../download/bundlers">Vendors that Distribute Audacity</a>). Here are guidelines for bundling, reselling, or distributing Audacity. If you adhere to these guidelines, please <a href="../contact/">contact us</a> for possible inclusion at <a href="../download/bundlers">Vendors that Distribute Audacity</a> (per instructions there).</p>

<p>You must include, in accordance with Section 3 of the GPL:
  <ol>
    <li>a copy of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and </li>
    <li>the <a href="http://audacity.sourceforge.net/download/source">Audacity source code</a> (or if you are distributing a modified version, then that source code), or make a written offer to supply the code.</li>
  </ol>
</p>

<p>If you distribute our installers and do not modify them, this will comply with both requirements as these include the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and an offer in README.txt for users to obtain the source code from us. Versions of Audacity after 1.3.3 also include the GPL in the application itself, in the About Audacity dialog.</p>

<p>We would appreciate your linking to our website: <a href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> so your users can download updated versions of Audacity when released. Also, please subscribe to the notifications list at the bottom of <a href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> to ensure you are notified of and can distribute the latest version of Audacity.</p>

<p>If possible, please include the Audacity documentation from <a href="http://audacity.sourceforge.net/help/documentation">http://audacity.sourceforge.net/help/documentation</a> with your distribution, to reduce the number of queries we receive. Also, please make it clear in your documentation that the Audacity Team cannot offer support on your own your product, only on Audacity. </p>

<p>IMPORTANT: If you write your own documentation on Audacity, please let us review it for possible errors, <em>before</em> you publish it.</p>
   
<p>If you modify Audacity without the express permission of the Audacity Team, you do not have the right to use the name Audacity, which is registered trademark of Dominic Mazzoni.  You must rename the product and provide all of your own technical support.</p>

<p>
  <dl><dt></dt><dd><!-- indent cheat -->
    <h4>For vendors selling a hardware product, with Audacity included:</h4>
  </dd></dl>
  <ol>
    <li>Thank you for choosing to distribute Audacity with your product! Please <a href="../contact/">contact us</a> early so that we can work together to help make your product a success.</li>
    <li>Please distribute Audacity unmodified. If you need to modify it to work with your device, please <a href="../contact/">contact us</a> first.</li>
    <li>Please test your device with Audacity thoroughly.  If there are any specific configuration instructions that users need to follow in order for your device to work with Audacity, make sure they are included with your device.</li>
    <li>Please do not refer customers to Audacity technical support when they are having difficulties using your device.  If you are selling a commercial product, you should provide your own technical support.  When users are having Audacity-specific issues that are not related to your specific device, we will of course be happy to assist them.</li>
  </ol>
</p>

<p>
  <dl><dt></dt><dd><!-- indent cheat -->
    <h4>For vendors who wish to resell or distribute Audacity:</h4>
  </dd></dl>
  <ol>
    <li>If you are reselling Audacity unmodified, your advertising should make it clear that Audacity is available for free on the Internet, and make it clear what the customer is paying for when they purchase Audacity from you.  Possibilities include the convenience of Audacity on CD-ROM, other bundled software or music files, a printed manual, or more.</li>
    <li>If you are reselling Audacity unmodified, we expect you to provide your own technical support wherever possible, in support of your own customers. If you have any concerns or we can help you in this, please <a href="../contact/">contact us</a>.</li>
  </ol>
</p>

<p>You may not advertise yourself or your business as an authorized distributor of Audacity. The name Audacity is a registered trademark of Dominic Mazzoni.</p>

<p>See <a href="http://audacityteam.org/wiki/index.php?title=AudacityVendors#Advice_for_Vendors">Advice for Vendors</a> on the <a href="http://audacityteam.org/wiki">Audacity Wiki</a> for further advice.</p>');
?>

<!-- TODO: Copyright and licenses for libraries. -->

<?php
  include "../include/footer.inc.php";
?>
