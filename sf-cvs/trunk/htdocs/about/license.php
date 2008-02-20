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
//   http://www.gnu.org/licenses/licenses.es.html#GPL
//   http://www.gnu.org/licenses/gpl-faq.cs.html
//
// See the bottom of each page for versions in other languages.
//
echo _('<h3 id="license">License</h3>

<p>Audacity is <a href="../help/faq?s=general&amp;i=free">free software</a>. You may use it for any personal, 
commercial or educational purpose, including installing it on as many different computers as you wish.</p>

<p>You may also copy, distribute, modify, and/or resell Audacity, under the terms of the <a
href="http://www.gnu.org/licenses/licenses.html#GPL">GNU General Public License (GPL)</a> as published by the Free
Software Foundation - either <a href="http://www.gnu.org/licenses/old-licenses/gpl-2.0.html">version 2 of the 
License</a>, or (at your option) any later version. In granting you this
right, the GPL requires that the source code you distribute is itself available under the GPL. If you have
questions, please <a
href="&#109;&#x61;&#105;&#108;&#x74;&#x6f;&#58;&#x61;&#x75;&#100;&#97;&#x63;&#105;&#x74;&#121;&#x2d;&#x66;&#101;&#101;&#x64;&#98;&#97;&#x63;&#x6b;&#64;&#x6c;&#105;&#115;&#x74;&#115;&#x2e;&#x73;&#111;&#x75;&#x72;&#x63;&#x65;&#x66;&#111;&#x72;&#x67;&#x65;&#x2e;&#110;&#x65;&#116;&#63;&#115;&#x75;&#x62;&#x6a;&#x65;&#x63;&#116;&#61;&#x4c;&#x69;&#99;&#101;&#110;&#115;&#x69;&#x6e;&#x67;&#x20;&#69;&#x6e;&#113;&#117;&#105;&#x72;&#x79;">e-mail
us</a>. <b>Note:</b> In line with our <a href="../contact/privacy">Privacy Policy</a>, this address is a public 
mailing list - your message will be seen by subscribers to the list, and will be visible on several web sites where 
the messages are archived.</p>

<p>See also: <a href="http://www.gnu.org/licenses/gpl-faq.html">Frequently Asked Questions about the GNU 
GPL</a>.</p>');


echo _('<h3 id="advice">Advice for Vendors and Distributors</h3>

<p>A variety of <a href="../download/bundlers#bundlers">companies and individuals</a> bundle Audacity with
their products, resell or otherwise distribute it. Below are requirements and guidelines for bundling, reselling or 
distributing
Audacity. If you adhere to these, please <a 
href="&#x6d;&#x61;&#x69;&#x6c;&#116;&#x6f;&#x3a;&#118;&#97;&#117;&#103;&#104;&#97;&#x6e;&#x40;&#97;&#x75;&#100;&#97;&#99;&#x69;&#x74;&#x79;&#x74;&#x65;&#97;&#x6d;&#46;&#x6f;&#x72;&#x67;&#x3f;&#x73;&#x75;&#x62;&#106;&#x65;&#x63;&#116;&#61;&#x42;&#x75;&#110;&#x64;&#x6c;&#101;&#114;&#115;&#32;&#x70;&#x61;&#x67;&#101;&#32;&#x65;&#110;&#113;&#x75;&#x69;&#x72;&#x79;">e-mail 
us</a> for possible inclusion on our <a href="../download/bundlers">Vendors and Distributors of Audacity</a> 
page.</p>');

echo _('<h4 >Legal Requirements</h4>

<p>You <b>must</b> include the following to comply with version 2 or later of the 
<a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a>:
  <ol>
    <li>a verbatim copy of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and </li>
    <li>the <a href="http://audacity.sourceforge.net/download/source">Audacity source code</a> (or if you are 
distributing a modified version of Audacity, that modified code). Failing that, you must include a written offer to 
supply the code.</li>
  </ol>
</p>');

echo _('<p>If 
you distribute our <a href="http://audacity.sourceforge.net/download/">installers</a> and do not modify them, 
this will fully comply, because the installers include the <a 
href="http://www.gnu.org/licenses/licenses.html#GPL">GPL</a> and an offer in README.txt for users to obtain the 
source code from us. Versions of Audacity after 1.3.3 also include the GPL in the application itself, in the About 
Audacity dialog.</p>');

echo _('<p>Additionally, the name <b>Audacity</b>&reg; is a <b>registered trademark</b> of Dominic Mazzoni. Therefore 
you <b>may not</b> advertise yourself or your business as an authorized distributor of Audacity, and you <b>may not</b> modify Audacity then continue to use the Audacity name without the express permission of the Audacity Team.</p>');

echo _('<h4 >Updating and Documenting Audacity</h4>

<p>We would appreciate your linking to our website: <a 
href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> so your users can download updated 
versions of Audacity when released. Also, please subscribe to the notifications list at the bottom of <a 
href="http://audacity.sourceforge.net">http://audacity.sourceforge.net</a> to ensure you receive advice of and can 
distribute the latest version of Audacity.</p>');

echo _('<p>If possible, please include the <a href="http://audacity.sourceforge.net/help/documentation">Audacity 
documentation</a> with your distribution, to reduce the number of queries we receive. IMPORTANT: If you write your 
own documentation for Audacity, please <a 
href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#x3a;&#x76;&#97;&#x75;&#x67;&#x68;&#x61;&#110;&#x40;&#97;&#117;&#100;&#x61;&#x63;&#105;&#x74;&#121;&#116;&#x65;&#97;&#109;&#46;&#111;&#x72;&#x67;&#x3f;&#115;&#x75;&#98;&#106;&#x65;&#99;&#116;&#x3d;&#65;&#117;&#x64;&#x61;&#99;&#105;&#x74;&#121;&#x20;&#68;&#x6f;&#99;&#x75;&#x6d;&#101;&#110;&#x74;&#97;&#116;&#105;&#111;&#110;">submit 
it to us</a> for review <em>before</em> you publish it.</p>');

echo _('<h4>For vendors selling a hardware product, with Audacity included:</h4>

<p>
   <ol>
    <li>Thank you for choosing to distribute Audacity with your product! Please <a 
href="&#x6d;&#97;&#105;&#108;&#x74;&#x6f;&#x3a;&#97;&#117;&#x64;&#x61;&#x63;&#x69;&#x74;&#x79;&#x2d;&#102;&#x65;&#101;&#100;&#x62;&#x61;&#99;&#107;&#64;&#108;&#105;&#x73;&#116;&#x73;&#x2e;&#x73;&#111;&#x75;&#x72;&#99;&#x65;&#x66;&#111;&#114;&#x67;&#101;&#x2e;&#110;&#x65;&#116;&#x3f;&#x73;&#117;&#98;&#x6a;&#x65;&#99;&#x74;&#61;&#66;&#x75;&#x6e;&#100;&#108;&#x65;&#x72;&#115;&#32;&#69;&#x6e;&#113;&#x75;&#105;&#x72;&#x79;">contact 
us</a> early so that we can work together to make your product a success.</li>
    <li>Please distribute Audacity unmodified. If you need to modify it to work with your device, please <a 
href="&#x6d;&#97;&#105;&#108;&#x74;&#x6f;&#x3a;&#97;&#117;&#x64;&#x61;&#x63;&#x69;&#x74;&#x79;&#x2d;&#102;&#x65;&#101;&#100;&#x62;&#x61;&#99;&#107;&#64;&#108;&#105;&#x73;&#116;&#x73;&#x2e;&#x73;&#111;&#x75;&#x72;&#99;&#x65;&#x66;&#111;&#114;&#x67;&#101;&#x2e;&#110;&#x65;&#116;&#x3f;&#x73;&#117;&#98;&#x6a;&#x65;&#99;&#x74;&#61;&#66;&#x75;&#x6e;&#100;&#108;&#x65;&#x72;&#115;&#32;&#69;&#x6e;&#113;&#x75;&#105;&#x72;&#x79;">contact 
us</a> first.</li>
    <li>Please test your device with Audacity thoroughly.  If there are any specific configuration instructions 
needed to make your device work with Audacity, please ensure these are included with the device.</li>
    <li>Please note that we cannot help your customers with problems specific to your device - these must 
be dealt with by your own qualified staff. Users needing support with Audacity itself can find assistance at <a 
href="http://audacity.sourceforge.net/contact/">http://audacity.sourceforge.net/contact/</a>.</li>
  </ol>
</p>');

echo _('<h4>For those wishing to resell Audacity, or a rebranded version of it:</h4>

<p>
 <ol>
    <li>If you are reselling Audacity unmodified, your advertising should make it clear that Audacity is available 
for free on the Internet, and highlight the benefits of the customer purchasing Audacity from you.  For example. you 
could offer Audacity on a CD-ROM, bundle other software or music files, or provide a printed manual.</li>
    <li>You should endeavour to provide your own technical support for your customers, and your own documentation if 
you have modified Audacity. If you are reselling Audacity exactly as obtained from us and have questions about 
this, please <a 
href="&#x6d;&#x61;&#x69;&#x6c;&#116;&#111;&#x3a;&#x61;&#x75;&#x64;&#x61;&#99;&#x69;&#116;&#x79;&#45;&#x66;&#x65;&#x65;&#100;&#x62;&#x61;&#x63;&#x6b;&#x40;&#x6c;&#x69;&#x73;&#x74;&#115;&#x2e;&#x73;&#x6f;&#x75;&#114;&#x63;&#x65;&#x66;&#111;&#x72;&#103;&#x65;&#x2e;&#110;&#101;&#x74;&#63;&#x73;&#x75;&#x62;&#x6a;&#x65;&#x63;&#x74;&#61;&#x52;&#101;&#x73;&#x65;&#x6c;&#108;&#101;&#114;&#x73;&#32;&#69;&#x6e;&#113;&#x75;&#x69;&#x72;&#x79;">e-mail 
us</a>.</li>
    <li>Please read these <a 
href="http://audacityteam.org/wiki/index.php?title=AudacityVendors#General_Advice">additional guidelines for 
resellers</a> on the <a href="http://audacityteam.org/wiki">Audacity Wiki</a>.</li>
  </ol>
</p>');

echo _('<p><b>Note:</b> All e-mails sent to lists.sourceforge.net addresses will be seen by subscribers to the list
and will be visible on several websites where the messages are archived. For details, see our <a 
href="../contact/privacy">Privacy Policy</a>.</p>');
?>

<!-- TODO: Copyright and licenses for libraries. -->

<?php
  include "../include/footer.inc.php";
?>
