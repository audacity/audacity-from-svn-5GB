<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * Copyright 2006-11 Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "donate";
  $pageTitle = _("How Does Audacity Raise Money?");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p>
  <?=_('Audacity raises money via <a href="#donate">donations</a>, <a href="http://audacityteam.org/sponsor.php">sponsorships</a> and <a href="../contact/privacy#advertising">advertisements</a> on our web pages.')?>
</p>

<p>
  <?=_('As of March 2007, <a href="http://sourceforge.net/">SourceForge.net</a>, 
  which hosts the Audacity site and downloads, is allowing us to also post 
  advertisements, in an innovative program whereby we split the revenues.
  Thank you, <a href="http://sourceforge.net/">SourceForge.net</a>!')?>
</p>

<p>
  <?=_('Income to Audacity is not distributed among previous contributors to Audacity. 
  There are too many people and it would be difficult to do so fairly. 
  Also, nobody contributed code to Audacity expecting money in return. 
  Audacity is developed by volunteers, working in our spare time. 
  Audacity is not shareware, and we do not expect to make any money from it.')?>
</p>

<p>
  <?=_('Rather, the team of Audacity lead developers will spend the money on items and 
  services that benefit the project as a whole. 
  Money that comes in to the project helps us pay for bandwidth and web hosting, 
  development tools, and audio hardware.
  In the past we used funds from donations to purchase the Audacity trademark and to
  subsidize travel costs for Audacity developers to meet for a hackathon, and we plan to
  do more of these.
  Our long-term goal is to bring in enough money to hire a full-time developer who could 
  devote all of his/her time to the project and take over some of the administrative, 
  maintenance and release tasks, freeing volunteers to spend more time on code.')?>
</p>

<p>
  <?=_('These are all things that help us provide Audacity for free and continue to make it better. 
  The Audacity Team is completely committed to keeping Audacity free and open source.
  We made that commitment in
  <a href="http://audacity.sourceforge.net/about/license">
      licensing Audacity under the GNU General Public License</a>.')?>
</p>

<p>
  <?=_('There are many ways to support Audacity, and many degrees of doing so.  
  Besides donating money, you can <a href="http://wiki.audacityteam.org/wiki/Contribute">
  contribute</a> by helping us with <a href="../community/developers">programming</a>,
  <a href="../contact#feedback">testing</a>, <a href="http://manual.audacityteam.org/">
  documentation</a>,<a href="../community/translation">translations</a> or by anwering
  questions on our <a href="http://forum.audacityteam.org/">Forum</a>. We have always 
  welcomed users whose only contribution is simply using Audacity, giving us feedback
  on how to improve it and telling others about it. Thank you for supporting Audacity.')?>
</p>

<h2 id="donate"><?=_('Make a Donation')?></h2>
<p id="paypal"><?=_('Click the button below to donate securely by Paypal (a Paypal account is not required). You can send any amount, using either your credit card or your Paypal account:')?></p>

<!-- Begin Paypal button Form --> 
<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<p class="logo">
   <input type="hidden" name="cmd" value="_s-xclick">
   <input type="hidden" name="item_name" value="Thank you for your donation to Audacity!">
   <input type="hidden" name="return" value="http://audacity.sourceforge.net/">
   <input type="hidden" name="cancel_return" value="http://audacity.sourceforge.net/community/donate">
   <input type="image" src="../images/Paypal_large_blue.png" border="0" name="submit" alt="Paypal logo" title="Donate securely using your credit card or Paypal account">
   <input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----">
</p>
</form>
<!-- End Paypal button Form -->

<p>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the two 
   // "echo" strings obscures the e-mail address from (at least some) 
   // harvesting bots. Please translate the strings above and below
   // the <a href> tag, and ignore the line with the <a href> tag itself.   
   echo _('If you prefer to send a personal check or another form of payment, please write to our ')?>
   <a href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#102;&#101;&#101;&#100;&#x62;&#97;&#99;&#107;&#x40;&#97;&#x75;&#x64;&#97;&#x63;&#105;&#116;&#x79;&#116;&#101;&#97;&#x6d;&#46;&#111;&#114;&#x67;"> 
<?php
   echo _('feedback e-mail address</a> telling us what country you are in and how you\'d like to pay.');?></p>


<?php
  include "../include/footer.inc.php";
?>