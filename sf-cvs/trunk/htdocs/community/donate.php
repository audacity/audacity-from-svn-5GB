<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * Copyright 2006 Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "donate";
  $pageTitle = _("How Does Audacity Raise Money?");
  include "../include/header_adsense.inc.php";
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
  Audacity-logo items (T-shirts, embroidered polo shirts, embroidered messenger bags), and consumer electronics.')?>
</p>
<p class="logo">
  <a title="<?=_('Audacity Store')?>" 
     href="http://audacitystore.com/">
    <img alt="<?=_('Audacity Store')?>" 
    src="../images/Audacity Store_banner_50pct.jpg">
  </a>
</p>


<h2><?= ('Make a Donation')?></h2>
<p>
  <?=_('We currently accept donations via the Amazon Honor System. Use your credit card and make a donation as small as US$1.00, or up to US$50.00:')?>
</p>

<!-- Begin Amazon Honor System Paybox -->
<p class="logo"><img src="http://s1.amazon.com/exec/varzea/tipbox/A2NTHQTXQE4A60/TVWQG2UJNWCBH" usemap="#amazon" alt="Amazon Honor System" width="150" height="150"></p>
<map name="amazon">
  <area coords="20,60,130,115" href="http://s1.amazon.com/exec/varzea/pay/TVWQG2UJNWCBH" alt="Click to Give">
  <area coords="5,135,145,145" href="http://s1.amazon.com/exec/varzea/subst/fx/help/how-we-know.html" alt="Learn More">
</map>
<!-- End Amazon Honor System Paybox -->

<p><?=_('We also accept PayPal donations through the SourceForge donation system:')?></p>

<p class="logo">
  <a title="<?=_('Donate via PayPal')?>" 
    href="http://sourceforge.net/donate/index.php?group_id=6235">
    <img alt="<?=_('Donate via PayPal')?>" 
    src="../images/paypal.gif"></a>
</p>

<p><?=_('If you prefer to send a personal check or another form of payment, please visit the <a href="../about/credits">credits</a> page and contact one of the lead developers.')?></p>


<h2>
  <?= ('Sponsor Audacity Development')?>
</h2>
<p><?=_('We would also like to hear from any companies or groups interested in <a href="http://audacityteam.org/sponsor.php">sponsoring Audacity development</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
