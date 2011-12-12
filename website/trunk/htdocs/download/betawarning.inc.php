<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2011 Martyn Shaw, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
?>

<h3><?=_('Beta version')?></h3>

<p>
<?=_('Audacity 1.3 Beta is our active version with the latest features and fixes. It is a mature Beta, intended to become the new 2.0 stable version in the near future. Because the current 1.2 stable version is now old, most users should download the Beta version. Please check the Beta <a href="#sysreq">System Requirements</a>.')?></p>

<p><?=_('
 <ul>
  <li>Contains dozens of')?> <a href="features-1.3-a#details"><?=_('new, exciting features.')?></a> <?=_('Very occasionally, these might need final polishing or not be retained in later versions.')?></li>
  <li><?=_('Occasionally, a feature might not work as it did before, or might be temporarily disabled.</li>
  <li>Some parts of the program are not yet documented or translated into different languages.</li>
 </ul>
')?></p>

<p>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the
   // two echo strings obscures the e-mail address from (at least
   // some) harvesting bots. Please translate the strings above
   // and below the <a href> tag, and ignore the line with the
   // <a href> tag itself.   
   echo _('Please help our development effort by trying the Beta and ')?>
   <a href="&#x6d;&#97;&#105;&#108;&#116;&#x6f;&#x3a;&#102;&#x65;&#x65;&#x64;&#98;&#x61;&#x63;&#107;&#x40;&#97;&#x75;&#100;&#x61;&#x63;&#x69;&#116;&#121;&#116;&#x65;&#x61;&#109;&#x2e;&#111;&#114;&#x67;&#63;&#x73;&#117;&#x62;&#x6a;&#x65;&#x63;&#116;&#61;&#x42;&#x65;&#x74;&#x61;&#95;&#70;&#101;&#x65;&#100;&#x62;&#x61;&#99;&#x6b;">
<?php
   echo _('send us your comments</a>. Please mention your operating system, full details of any problems, and any features that need improvement or no longer work properly. You can install 1.3 and 1.2 on the same machine by installing them to different directories.')?></p>

