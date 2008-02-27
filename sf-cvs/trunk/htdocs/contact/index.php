<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 Richard Ash
 * 2007 Vaughan Johnson
 * 2008 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Contact Us");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<?php
  // i18n-hint: If there are any special sources of help (local forums, lists) for Audacity
  // users in your language (other than our Russian, French, German and Spanish
  // forums), please add links to them here and at the bottom of \htdocs\help\index.php.
  // If there is any translated documentation, please add links to it at the bottom
  // of \htdocs\help\documentation.php
  ?>

<h3><?=_('Personal Support with Audacity Questions or Problems')?></h3>
<p><?=_('If you have not already done so, please check our <a href="../help/faq">Frequently Asked Questions (FAQ)</a> and <a href="../help/documentation">documentation</a>. Please also search our <a href="http://www.nabble.com/audacity-help-f4506.html">help-list archive</a>. Although this list is no longer active, the archive has extensive coverage of many issues.')?></p>
<p>
  <?php echo _('If you still need help, please visit our user');?>
  <a href="http://audacityteam.org/forum/"><?php
  // i18n-hint: "Forum" is the link text to the Forum. Probably you should not
  // translate this link text unless there is a sub-forum in your language.
  echo _('Forum</a>.');?>
  <?php
  echo _(' Search the Forum to see if you can find an immediate answer. If not, <a href="http://audacityteam.org/forum/ucp.php?mode=register">register</a> at the Forum, confirm your registration and then log in. Choose the correct subforum to post to (for example, the Windows forum for Audacity 1.2.x), click "New Topic", then submit your message. Foreign language subforums are available for <a href="http://audacityteam.org/forum/viewforum.php?f=7">French</a>, <a href="http://audacity-forum.de/">German</a>, <a href="http://audacityteam.org/forum/viewforum.php?f=10">Russian</a> and <a href="http://audacityteam.org/forum/viewforum.php?f=8">Spanish.</a>');?>
</p>
<p><?=_('When reporting a problem or apparent bug to the Forum, please include full details of what you are trying to do. If there is a crash or error message, please state what you did that led up to this, and what exactly any error message said.')?></p>

<h3><?=_('Suggestions and Comments for the Audacity Developers')?></h3>
<p><?=_('If you have feedback for us that does <b>not</b> require a personal response, please <a href="&#109;&#97;&#105;&#108;&#116;&#111;&#x3a;&#97;&#x75;&#100;&#x61;&#x63;&#105;&#116;&#x79;&#x2d;&#102;&#101;&#101;&#x64;&#x62;&#x61;&#99;&#x6b;&#64;&#108;&#x69;&#115;&#x74;&#x73;&#x2e;&#x73;&#x6f;&#117;&#114;&#x63;&#x65;&#102;&#x6f;&#114;&#x67;&#101;&#x2e;&#x6e;&#x65;&#116;&#32;">e-mail us</a>. Please tell us your experiences of the Audacity program, documentation or web site. We particularly welcome reports of possible program bugs and suggestions for new Audacity features.')?></p>
<p><?=_('Reports of apparent bugs should be as specific as possible, including:</p>
<ol>
  <li>Your version of Audacity (for example, 1.2.6).</li>
  <li>Your operating system (for example, Windows XP Service Pack 2 or Intel Mac OS X 10.4).</li>
  <li>Details of what you were trying to do, what steps led to the problem occurring, and details of any error messages.</li>
</ol>

<p>We can only accept feedback in English at present. <b>Note:</b> In line with our <a href="../contact/privacy">Privacy Policy</a>, this address is a public mailing list - messages are seen by all list subscribers, and posted on several web sites where the messages are archived.</p>')?>

<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('To discuss Audacity with other users and developers, subscribe to our');?>
 <a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a>.</p>
<?php
  include "../include/footer.inc.php";
?>
