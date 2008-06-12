<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 Richard Ash
 * 2007 Vaughan Johnson
 * 2008 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
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
  <a href="http://audacityteam.org/forum/">
<?php
  // i18n-hint: "Forum" is the link text to the Forum. Probably you should not
  // translate this link text unless there is a sub-forum in your language.
  echo _('Forum</a>.');?>
<?php
  echo _(' Search the Forum to see if you can find an immediate answer. If not, <a href="http://audacityteam.org/forum/ucp.php?mode=register">register</a> at the Forum, confirm your registration and then log in. Choose the correct subforum to post to (for example, the Windows forum for Audacity 1.2.x), click "New Topic", then submit your message. Foreign language subforums are available for <a href="http://audacityteam.org/forum/viewforum.php?f=7">French</a>, <a href="http://audacity-forum.de/">German</a>, <a href="http://audacityteam.org/forum/viewforum.php?f=10">Russian</a> and <a href="http://audacityteam.org/forum/viewforum.php?f=8">Spanish</a>.');?>
</p>
<p><?=_('When reporting a problem or apparent bug to the Forum, please include full details of what you are trying to do. If there is a crash or error message, please state what you did that led up to this, and what exactly any error message said.')?></p>

<h3 id="feedback"><?=_('Suggestions and Comments for the Audacity Developers')?></h3>
<p>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the two 
   // "echo" strings obscures the e-mail address from (at least some) 
   // harvesting bots. Please translate the strings above and below
   // the <a href> tag, and ignore the line with the <a href> tag itself.   
   echo _('If you have feedback for us that does <b>not</b> require a personal response, please ')?>
   <a href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#102;&#101;&#101;&#100;&#x62;&#97;&#99;&#107;&#x40;&#97;&#x75;&#x64;&#97;&#x63;&#105;&#116;&#x79;&#116;&#101;&#97;&#x6d;&#46;&#111;&#114;&#x67;"> 
<?php
   echo _('e-mail us</a>. Please tell us your experiences of the Audacity program, documentation or web site. We particularly welcome reports of possible program bugs and suggestions for new Audacity features.');?></p>

<p><?=_('Reports of apparent bugs should be as specific as possible, including:</p>
<ol>
  <li>Your version of Audacity (for example, 1.2.6).</li>
  <li>Your operating system (for example, Windows XP Service Pack 2 or Intel Mac OS X 10.4).</li>
  <li>Details of what you were trying to do, what steps led to the problem occurring, and details of any error messages.</li>
</ol>

<p><b>Note:</b> We can only accept feedback in English at present.</p>')?>

<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('Subscribe to our');?>
 <a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a> <?=_('to discuss Audacity with our community of users and developers. C++ developers: join ')?> <a href="https://lists.sourceforge.net/lists/listinfo/audacity-devel"> <?=_('our developers\' list')?></a> <?=_('to learn about the Audacity source code and contribute to making Audacity even better!')?></p>
<?php
  include "../include/footer.inc.php";
?>