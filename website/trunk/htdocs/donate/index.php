<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 - 12 Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("How Does Audacity Raise Money?");
  include "../include/header.inc.php";
?>

<h2 id="donate">
  <?=_('Make a Donation')?>
</h2>
<p>
  <?=_('Click the button below to donate securely by Paypal (a Paypal account is not required). You can send any amount, using either your credit card or your Paypal account:')?>
</p>

<p>
<center>
<!-- Begin Paypal button Form -->
<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
  <input type="hidden" name="cmd" value="_s-xclick">
    <input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----
">
      <input type="image" src="../images/Paypal_US_btn_donateCC_LG.gif" border="0" name="submit" alt ="Donate securely by Paypal, using your credit card or your Paypal account!" title="Donate securely by Paypal, using your credit card or your Paypal account!">
        <img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1">
</form>
<!-- End Paypal button Form -->
</center>
</p>

<p>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the two 
   // "echo" strings obscures the e-mail address from (at least some) 
   // harvesting bots. Please translate the strings above and below
   // the <a href> tag, and ignore the line with the <a href> tag itself.   
   echo _('If you prefer to send a personal check or another form of payment, please write to our ')?>
   <a href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#102;&#101;&#101;&#100;&#x62;&#97;&#99;&#107;&#x40;&#97;&#x75;&#x64;&#97;&#x63;&#105;&#116;&#x79;&#116;&#101;&#97;&#x6d;&#46;&#111;&#114;&#x67;"> 
<?php
   echo _('feedback e-mail address</a> telling us what country you are in and how you\'d like to pay.');?>
</p>

<h2><?=$pageTitle?></h2>

<p>
  <?=_('Audacity raises money via donations, <a href="http://audacityteam.org/sponsor.php">sponsorships</a> and <a href="../contact/privacy#advertising">advertisements</a> on our web pages.')?>
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

<h2><?=_('Other ways to contribute')?></h2>

<p>
  <?=_('Besides donating money, you can <a href="http://wiki.audacityteam.org/wiki/Contribute">
  contribute your time</a> by helping us with <a href="../community/developers">programming</a>,
  <a href="../contact#feedback">testing</a>, <a href="http://manual.audacityteam.org/">
  documentation</a>, <a href="../community/translation">translations</a> or by anwering
  questions on our <a href="http://forum.audacityteam.org/">Forum</a>. We have always 
  welcomed users whose only contribution is simply using Audacity, giving us feedback
  on how to improve it and telling others about it. Thank you for supporting Audacity.')?>
</p>

<?php
  include "../include/footer.inc.php";
?>
