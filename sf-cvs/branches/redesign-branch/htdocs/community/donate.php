<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "donate";
  $pageTitle = _("Make a Donation");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('Audacity is developed by volunteers, working in our spare time. It is not shareware, and we do not expect to make any money from it.  However, by donating money you can help us pay for bandwidth and web hosting, development tools, and audio hardware&mdash;things that help us provide Audacity for free and continue to make it better.')?></p>

<p><?=_('For a special gift if you donate US$20 or more, see our <a href="http://audacityteam.org/tshirt.php">Audacity T-shirt offer</a>.')?></p>

<p><?=_('We currently accept money via the Amazon Honor System. Use your credit card and make a payment as small as US$1.00, or up to US$50.00:')?></p>

<!-- Begin Amazon Honor System Paybox -->
<p class="logo"><img src="http://s1.amazon.com/exec/varzea/tipbox/A2NTHQTXQE4A60/TVWQG2UJNWCBH" usemap="#amazon" alt="Amazon Honor System" width="150" height="150"></p>
<map name="amazon">
  <area coords="20,60,130,115" href="http://s1.amazon.com/exec/varzea/pay/TVWQG2UJNWCBH" alt="Click to Give">
  <area coords="5,135,145,145" href="http://s1.amazon.com/exec/varzea/subst/fx/help/how-we-know.html" alt="Learn More">
</map>
<!-- End Amazon Honor System Paybox -->

<p><?=_('We also accept PayPal payments through the SourceForge donation system:')?></p>

<p class="logo">
  <a title="<?=_('Donate via PayPal')?>" href="http://sourceforge.net/donate/index.php?group_id=6235"><img alt="<?=_('Donate via PayPal')?>" src="../images/paypal.gif"></a>
</p>

<p><?=_('If you prefer to send a personal check or another form of payment, please visit the <a href="../about/credits">credits</a> page and contact one of the lead developers.')?></p>

<p><?=_('We would also like to hear from any companies or groups interested in <a href="http://audacityteam.org/sponsor.php">sponsoring Audacity development</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
