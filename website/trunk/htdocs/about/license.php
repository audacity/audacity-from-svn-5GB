<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Dominic Mazzoni,Vaughan Johnson
 * Copyright 2008 Gale Andrews
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
   // http://www.gnu.org/licenses/licenses.es.html#GPL
   // http://www.gnu.org/licenses/gpl-faq.cs.html
   //
   // See the bottom of each page for versions in other languages.
   //
echo _('<h3 id="license">License</h3>

<p>Audacity is <a href="../help/faq?s=general&amp;i=free">free software</a>. You may use it for any personal, commercial or educational purpose, including installing it on as many different computers as you wish.</p>

<p>You may also copy, distribute, modify, and/or resell Audacity, under the terms of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GNU General Public License (GPL)</a> as published by the Free Software Foundation - either <a href="http://www.gnu.org/licenses/old-licenses/gpl-2.0.html">version 2 of the License</a>, or (at your option) any later version. In granting you this right, the GPL requires that the source code you distribute is itself available under the GPL. ');?>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the two 
   // "echo" strings here and elsewhere on this page obscures the 
   // email address from (at least some) harvesting bots. Only the two 
   // strings above and below the <a href> tag need to be translated.   
   echo _('If you have questions, please ');?>
   <a href="&#x6d;&#x61;&#105;&#x6c;&#x74;&#x6f;&#x3a;&#x67;&#97;&#x6c;&#x65;&#64;&#97;&#117;&#100;&#x61;&#99;&#x69;&#x74;&#x79;&#x74;&#x65;&#x61;&#109;&#x2e;&#111;&#x72;&#103;&#x3f;&#x73;&#x75;&#x62;&#106;&#x65;&#x63;&#116;&#x3d;&#x4c;&#105;&#99;&#101;&#110;&#x73;&#105;&#110;&#103;&#x20;&#69;&#x6e;&#113;&#117;&#105;&#x72;&#x79;">
<?php
   echo _('e-mail us.');?>
   </a>
</p>

<p><?=_('See also <a href="http://www.gnu.org/licenses/gpl-faq.html">Frequently Asked Questions about the GNU GPL</a>.')?></p>

<?=_('<h3 id="advice">Advice for Vendors and Distributors</h3>

<p>A variety of companies and individuals bundle Audacity with their products, resell or otherwise distribute it. Below are requirements and guidelines for bundling, reselling or distributing Audacity. ')?>

<?php
   echo _('If you adhere to these, please ');?>
   <a href="&#x6d;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#103;&#97;&#108;&#101;&#64;&#97;&#117;&#x64;&#97;&#99;&#105;&#116;&#121;&#x74;&#x65;&#97;&#x6d;&#x2e;&#111;&#x72;&#x67;&#63;&#x73;&#x75;&#x62;&#106;&#101;&#99;&#116;&#x3d;&#66;&#x75;&#110;&#x64;&#108;&#101;&#x72;&#x73;&#x20;&#80;&#97;&#103;&#101;&#32;&#x45;&#x6e;&#113;&#117;&#105;&#x72;&#121;">
<?php
   echo _('e-mail us</a> for possible inclusion on our <a href="../download/bundlers">Vendors and Distributors of Audacity</a> page.');?>
</p>

<?php
echo _('<h4 >Legal Requirements</h4>

<p>You <b>must</b> include the following to comply with version 2 or later of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a>:
  <ol>
    <li>a verbatim copy of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and </li>
    <li>the <a href="http://audacity.sourceforge.net/download/source">Audacity source code</a> (or if you are distributing a modified version of Audacity, that modified code). Failing that, you must include a written offer to supply the code.</li>
  </ol>
</p>');

echo _('<p>If you distribute our <a href="http://audacity.sourceforge.net/download/">installers</a> and do not modify them, this will fully comply, because the installers include the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and an offer in README.txt for users to obtain the source code from us. Versions of Audacity after 1.3.3 also include the GPL in the application itself, in the About Audacity dialog.</p>');

echo _('<p>Additionally, the name <b>Audacity</b>&reg; is a <b>registered trademark</b> of Dominic Mazzoni. Therefore you <b>may not</b> advertise yourself or your business as an authorized distributor of Audacity, and you <b>may not</b> modify Audacity then continue to use the Audacity name without the express permission of the Audacity Team.</p>');

echo _('<h4 >Updating and Documenting Audacity</h4>

<p>We would appreciate your linking to our web site: <a href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> so your users can download updated versions of Audacity when released. Also, please subscribe to the notifications list at the bottom of <a href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> to ensure you receive advice of and can distribute the latest version of Audacity.</p>');

echo _('<p>If possible, please include the <a href="http://audacity.sourceforge.net/help/documentation">Audacity documentation</a> with your distribution, to reduce the number of queries we receive. ');?>

<?php
   echo _('IMPORTANT: If you write your own documentation for Audacity, please ');?>
   <a href="&#x6d;&#x61;&#x69;&#x6c;&#x74;&#111;&#x3a;&#x67;&#97;&#x6c;&#101;&#x40;&#97;&#x75;&#x64;&#x61;&#x63;&#105;&#116;&#x79;&#116;&#x65;&#x61;&#x6d;&#46;&#111;&#x72;&#x67;&#63;&#x73;&#117;&#x62;&#x6a;&#101;&#99;&#116;&#x3d;&#65;&#x75;&#x64;&#97;&#x63;&#x69;&#x74;&#x79;&#32;&#68;&#x6f;&#x63;&#117;&#x6d;&#101;&#110;&#x74;&#97;&#x74;&#x69;&#x6f;&#110;">
<?php
   echo _('submit it to us</a> for review <em>before</em> you publish it.</p>');

echo _('<h4>For vendors selling a hardware product, with Audacity included:</h4>');?>

<p>
   <ol>

<?php
   echo _('<li>Thank you for choosing to distribute Audacity with your product! Please ');?>
   <a href="&#x6d;&#97;&#105;&#x6c;&#116;&#x6f;&#58;&#103;&#97;&#108;&#x65;&#64;&#x61;&#117;&#x64;&#97;&#99;&#x69;&#116;&#121;&#116;&#101;&#x61;&#x6d;&#46;&#x6f;&#114;&#x67;&#x3f;&#x73;&#117;&#x62;&#x6a;&#101;&#99;&#116;&#61;&#66;&#x75;&#110;&#100;&#108;&#x65;&#114;&#115;&#32;&#69;&#110;&#x71;&#117;&#105;&#114;&#121;">
<?php
   echo _('contact us</a> early so we can work together to make your product a success.</li>');?>

<?php
   echo _('<li>Please distribute Audacity unmodified. If you need to modify it to work with your device, please ');?>
   <a href="&#x6d;&#97;&#105;&#x6c;&#116;&#x6f;&#58;&#103;&#97;&#108;&#x65;&#64;&#x61;&#117;&#x64;&#97;&#99;&#x69;&#116;&#121;&#116;&#101;&#x61;&#x6d;&#46;&#x6f;&#114;&#x67;&#x3f;&#x73;&#117;&#x62;&#x6a;&#101;&#99;&#116;&#61;&#66;&#x75;&#110;&#100;&#108;&#x65;&#114;&#115;&#32;&#69;&#110;&#x71;&#117;&#105;&#114;&#121;">
<?php
   echo _('contact us</a> first.</li>');?>

<?php
echo _('<li>Please test your device with Audacity thoroughly. If there are any specific configuration instructions needed to make your device work with Audacity, please ensure these are included with the device.</li>
        <li>Please note that we cannot help your customers with problems specific to your device - these must be dealt with by your own qualified staff. Users needing support with Audacity itself can find assistance at <a href="http://audacity.sourceforge.net/contact/">http://audacity.sourceforge.net/contact/</a>.</li>
  </ol>
</p>');

echo _('<h4>For those wishing to resell Audacity, or a rebranded version of it:</h4>

<p>
  <ol>
    <li>If you are reselling Audacity unmodified, your advertising should make it clear that Audacity is available for free on the Internet, and highlight the benefits of the customer purchasing Audacity from you.  For example, you could offer Audacity on a CD-ROM, bundle other software or music files, or provide a printed manual.</li>
    <li>You should endeavour to provide your own technical support for your customers, and your own documentation if you have modified Audacity. ');?>

<?php
   echo _('If you are reselling Audacity exactly as obtained from us and have questions about this, please ');?>
   <a href="&#x6d;&#x61;&#x69;&#x6c;&#x74;&#x6f;&#58;&#103;&#97;&#108;&#x65;&#x40;&#x61;&#x75;&#100;&#97;&#99;&#105;&#x74;&#x79;&#x74;&#x65;&#x61;&#x6d;&#46;&#111;&#x72;&#x67;&#x3f;&#x73;&#x75;&#x62;&#x6a;&#101;&#x63;&#116;&#x3d;&#82;&#x65;&#x73;&#x65;&#108;&#108;&#x65;&#x72;&#115;&#x20;&#x45;&#x6e;&#x71;&#117;&#105;&#114;&#x79;">
<?php
   echo _('e-mail us</a>.');

echo _('<li>Please read these <a href="http://audacityteam.org/wiki/index.php?title=AudacityVendors#General_Advice">additional guidelines for resellers</a> on the <a href="http://audacityteam.org/wiki">Audacity Wiki</a>.</li>
  </ol>
</p>');
?>

<!-- TODO: Copyright and licenses for libraries. -->

<?php
  include "../include/footer.inc.php";
?>