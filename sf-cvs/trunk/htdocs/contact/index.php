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
  // i18n-hint:  If there is an Audacity forum or mailing
  // list in your language, you may add a link to it below.
  // Note that the Audacity Forum has subforums for Spanish,
  // French, German, and Russian!
  ?>

<h3><?=_('Personal Support with Audacity Questions or Problems')?></h3>
<p><?=_('First, please check our <a href="../help/faq">Frequently Asked Questions (FAQ)</a>.')?></p>
<p>
  <?php echo _('If you still have a question about how to use Audacity or problems doing so, please visit our user ');?>
  <a href="http://audacityteam.org/forum/"><?php
  // i18n-hint: This is the link text to the Forum.
  echo _('Forum');?>
  </a><?php
  // i18n-hint: These are instructions on how to use the forum. The name of the
  // link probably shouldn't be translated unless the forum interface supports
  // your language.
  echo _('. You can search the Forum and may be able to find an immediate answer. If not, register at the Forum, confirm your registration and then log in. Choose the correct subforum to post to (for example, the Windows forum for Audacity 1.2.x), click "New Topic", then submit your message. Foreign language subforums are available for French, German, Russian and Spanish.');?>
</p>
<p><?=_('If you are reporting a problem or apparent bug, please include details of what you are trying to do. If there is a crash or error message, please state what you did that led up to this, and what exactly any error message said.')?></p>
<p><?=_('Helpful information can also be found by searching our old <a href="http://sourceforge.net/mailarchive/forum.php?forum_name=audacity-help">-help list archive</a>, also available at <a href="http://www.nabble.com/audacity-help-f4506.html">Nabble</a> with easier search.')?></p>

<h3><?=_('Suggestions and Comments for the Audacity Developers')?></h3>
<p><?=_('If you have feedback for us that does <b>not</b> require a personal response, please email us at <a href="mailto:audacity-feedback@lists.sourceforge.net">audacity-feedback@lists.sourceforge.net</a>. Please tell us your experiences of the Audacity program, documentation or web site. We particularly welcome reports of possible program bugs and suggestions for new Audacity features.')?></p>
<p><?=_('Reports of apparent bugs should be as specific as possible, including:')?></p>
<ol>
  <li><?=_('Your version of Audacity (for example, 1.2.6).')?></li>
  <li><?=_('Your operating system (for example, Windows XP Service Pack 2 or Intel Mac OS X 10.4).')?></li>
  <li><?=_('Details of what you were trying to do, what steps led to the problem occurring, and details of any error messages.')?></li>
  </ol>

<p>We can only accept feedback in English at present. Please note, this is a public mailing list. For details, see our <a href="../contact/privacy">Privacy Policy</a>.</p>

<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('To discuss Audacity with other users and developers, subscribe to our');?>
 <a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a>.</p>
<?php
  include "../include/footer.inc.php";
?>
