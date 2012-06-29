<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2005-12 Matt Brubeck, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "privacy";
  $pageTitle = _("Privacy Policy");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3 id="advertising"><?=_("Advertisements Policy")?></h3>

<?=_('<p>On some pages on our main site, we show advertisements from Google\'s AdSense program. See our &ldquo;<a href="http://audacity.sourceforge.net/donate/">How Does Audacity Raise Money?</a>&rdquo; page for the reasons we show advertisements.</p>

<p>As of April 8, 2009, this includes "interest-based" advertising, which utilizes cookies to try to determine users\' areas of interest (for example, audio editing), to show advertisements of likelier interest. Please see Google\'s <a href="http://www.google.com/policies/privacy/ads/">Advertising privacy FAQ</a> for information regarding your privacy including how to <a href="http://www.google.com/policies/privacy/ads/#toc-optout">opt out</a> of this tracking.</p>')?>

<h3 id="announce"><?=_("Announcement List Policy")?></h3>

<?php
// Translators - please add a sentence which explains that these
// announcements will usually be in English only.  Possibly people can
// join users lists in their native languages later on, to hear
// announcements there.

echo _('<p>You can subscribe to our announcement list by entering your e-mail address in the <a href="../#announce"> form</a> on the <a href="../">Audacity home page</a>.  We will use this list only to send you brief announcements of new Audacity releases.  We will never share the addresses on this list with anyone.</p>
<p>To <b>unsubscribe</b> from the list at any time, enter your address in the <a href="../#announce">form</a> on our <a href="../">home page</a> and click "Remove". If you have any problems or questions, please e-mail our <a href="http://audacity.sourceforge.net/contact/#feedback">feedback address</a>.</p>')?>

<h3 id="lists"><?=_("Mailing List Policy")?></h3>

<?=_('<p>(This applies to <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a>, <a href="http://lists.sourceforge.net/lists/listinfo/audacity-users">audacity-users</a> and our other <a href="../contact/lists">discussion lists</a>.)</p>

<p>These are public mailing lists.  When you send a message to any of these addresses, it will be forwarded to all the list subscribers, including Audacity developers and others.  SourceForge and other web sites may publish archives of these lists.</p>
<p><b>Your e-mail address will always be truncated in published archives for your privacy, and the Audacity developers will not share your address with anyone.</b>  However, we <b>cannot</b> prevent other subscribers from seeing or publishing your messages. Therefore we recommend you avoid putting contact details such as street addresses or telephone numbers in your e-mails.</p>
<p>If you have any questions about this policy, please e-mail our <a href="http://audacity.sourceforge.net/contact/#feedback">feedback address</a>.</p>')?>


<?php
  include "../include/footer.inc.php";
?>