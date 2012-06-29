<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2005-9 Matt Brubeck, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "privacy";
  $pageTitle = _("Privacy Policy");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<!-- Translators - please add a sentence which explains that these
 announcements will usually be in English only.  Possibly people can
 join users lists in their native languages later on, to hear
 announcements there.
-->

<h3 id="advertising"><?=_("Advertisements Policy")?></h3>

<?=_('<p>On some pages on our main site, we show advertisements from Google\'s AdSense program. See our &ldquo;<a href="http://audacity.sourceforge.net/community/donate">How Does Audacity Raise Money?</a>&rdquo; page for the reasons we show advertisements.</p>

<p>As of April 8, 2009, this includes "interest-based" advertising, which utilizes cookies to try to determine users\' areas of interest (for example, audio editing), to show advertisements of likelier interest. Please see <a href="https://www.google.com/adsense/support/bin/answer.py?hl=en&answer=100557">Google Advertising Cookie and Privacy Policies</a> for information regarding your privacy and how to <a href="http://www.google.com/privacy_ads.html">opt out</a> of this tracking.</p>')?>

<h3 id="announce"><?=_("Announcement List Policy")?></h3>

<?=_('<p>You can subscribe to our announcement list by entering your e-mail address in the <a href="../#announce"> form</a> on the <a href="../">Audacity home page</a>.  We will use this list only to send you brief announcements of new Audacity releases.  We will never share the addresses on this list with anyone.</p>
<p>To <b>unsubscribe</b> from the list at any time, enter your address in the <a href="../#announce">form</a> on our <a href="../">home page</a> and click "Remove". If you have any problems or questions, please contact our <a href="&#x6d;&#x61;&#105;&#108;&#x74;&#111;&#x3a;&#100;&#x6f;&#109;&#x69;&#110;&#x69;&#x63;&#64;&#x61;&#117;&#100;&#97;&#99;&#x69;&#x74;&#121;&#x74;&#x65;&#x61;&#109;&#x2e;&#111;&#114;&#103;&#63;&#115;&#117;&#98;&#x6a;&#101;&#99;&#116;&#61;&#x41;&#110;&#110;&#111;&#x75;&#110;&#99;&#x65;&#109;&#101;&#x6e;&#x74;&#32;&#108;&#105;&#x73;&#116;">lead developer</a>.</p>')?>

<h3 id="lists"><?=_("Mailing List Policy")?></h3>

<?=_('<p>(This applies to <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a>, <a href="http://lists.sourceforge.net/lists/listinfo/audacity-users">audacity-users</a> and our other <a href="../contact/lists">discussion lists</a>.)</p>

<p>These are public mailing lists.  When you send a message to any of these addresses, it will be forwarded to all the list subscribers, including Audacity developers and others.  SourceForge and other web sites may publish archives of these lists.</p>
<p><b>Your e-mail address will always be truncated in published archives for your privacy, and the Audacity developers will not share your address with anyone.</b>  However, we <b>cannot</b> prevent other subscribers from seeing or publishing your messages. Therefore we recommend you avoid putting contact details such as street addresses or telephone numbers in your e-mails.</p>
<p>If you have any questions about this policy or are not comfortable writing to a public mailing list, please send a private e-mail to our <a href="&#109;&#x61;&#x69;&#108;&#x74;&#111;&#x3a;&#x64;&#111;&#x6d;&#105;&#x6e;&#x69;&#x63;&#64;&#97;&#x75;&#100;&#x61;&#99;&#105;&#116;&#121;&#x74;&#x65;&#97;&#109;&#x2e;&#x6f;&#x72;&#x67;&#x3f;&#115;&#x75;&#98;&#106;&#101;&#99;&#116;&#x3d;&#x4d;&#97;&#x69;&#108;&#105;&#110;&#x67;&#x20;&#76;&#x69;&#115;&#x74;&#115;">lead developer</a>.</p>')?>


<?php
  include "../include/footer.inc.php";
?>