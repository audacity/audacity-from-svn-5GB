<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Vaughan Johnson
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
  <?=_('Audacity raises money via donations, advertisements on our web pages, and 
  product sales through the <a href="http://audacitystore.com/">Audacity Store</a>.')?>
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
  maintenance, and release tasks, freeing volunteers to spend more time on code.')?>
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
  We have always welcomed users whose only contribution is simply using Audacity, 
  giving us feedback on how to improve it, and telling others about it. 
  Thank you for supporting Audacity.')?>
</p>


<h2>
  <?= ('Purchase from the <a href="http://audacitystore.com/">Audacity Store</a>')?>
</h2>
<p>
  <?=_('You are invited to try out the new <a href="http://audacitystore.com/">Audacity Store</a>, which features 
  Audacity-logo items (T-shirts, embroidered polo shirts and embroidered messenger bags).')?>
</p>
<p class="logo">
  <a title="<?=_('Audacity Store')?>"
     href="http://audacitystore.com/">
    <img alt="<?=_('Audacity Store')?>"
    src="../images/Audacity Store_banner_50pct.jpg">
  </a>
</p>


<h2 id="donate"><?= ('Make a Donation')?></h2>
<p>
  <?=_('We can accept donations via the Amazon Honor System, or by Paypal. To use Amazon, click the button below. You can make a donation securely by credit card, as little as US$1.00, or up to US$50.00:')?>
</p>

<!-- Begin Amazon Honor System Paybox -->
<p class="logo"><img src="http://s1.amazon.com/exec/varzea/tipbox/A2NTHQTXQE4A60/TVWQG2UJNWCBH" usemap="#amazon" alt="Amazon Honor System" width="150" height="150"></p>
<map name="amazon">
  <area coords="20,60,130,115" href="http://s1.amazon.com/exec/varzea/pay/TVWQG2UJNWCBH" alt="Click to Give">
  <area coords="5,135,145,145" href="http://s1.amazon.com/exec/varzea/subst/fx/help/how-we-know.html" alt="Learn More">
</map>
<!-- End Amazon Honor System Paybox -->

<p id="paypal"><?=_('Alternatively, click the button below to donate securely by Paypal. You can send any amount, using either your credit card or your Paypal account:')?></p>

<!-- Begin Paypal button Form --> 
<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<p class="logo">
   <input type="hidden" name="cmd" value="_s-xclick">
   <input type="hidden" name="item_name" value="Thank you for your donation to Audacity!">
   <input type="hidden" name="return" value="http://audacity.sourceforge.net/">
   <input type="hidden" name="cancel_return" value="http://audacity.sourceforge.net/community/donate">
   <input type="image" src="../images/Paypal_large_blue.png" border="0" name="submit" alt="Paypal logo">
   <input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----">
</p>
</form>
<!-- End Paypal button Form -->


<p><?=_('If you prefer to send a personal check or another form of payment, please visit the <a href="../about/credits">credits</a> page and contact one of the developers on the Technical Leadership Council.')?></p>


<h2>
  <?= ('Sponsor Audacity Development')?>
</h2>
<p><?=_('We would also like to hear from any companies or groups interested in <a href="http://audacityteam.org/sponsor.php">sponsoring Audacity development</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
