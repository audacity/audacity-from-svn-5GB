<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 Richard Ash
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
  // i18n-hint:  Please add a note that we can answer questions to
  // audacity-help in English only.  If there is an Audacity forum or mailing
  // list in your language, you may add a link to it below.
  // Note that the Audacity Forum has sections specially for Spanish,
  // French, German, and Russian!
  ?>
<p><?php  echo _('If you have questions or comments for the Audacity developers,
e-mail us at: ');?><a href="mailto:audacity-help@lists.sourceforge.net">
audacity-help@lists.sourceforge.net</a>. <?php
echo _('(This is a public mailing list.  For details, see our ');?><a href="../contact/privacy"><?php echo _('privacy policy');?></a>.)</p>

<p><?php echo _('If you have a general question about how to use Audacity,
please use the ');?><a href="http://audacityteam.org/forum/"><?php 
// i18n-hint: this is the link text to the Forum
echo _('Audacity Forum');?></a>.<?php 
// i18n-hint: this is instructions on how to use the forum. The name of the
// link probably shouldn't be translated unless the forum interface supports
// your language - currently German I think
echo _(' You will need to register in order to
post on the forum, then log in and use the "Create New Thread" link');?>.</p>

<p><b><?php echo _('When you report a bug or problem, please:');?></b></p>
<ol>
  <li><?php echo _('Before you contact us, check the ');?>
  <a href="../help/faq"><?php 
  // i18n-hint: this bit is the link text to the FAQ
  echo _('Frequently Asked Questions');?></a>.</li>
  <li><?php echo _('Tell us what version of Audacity and which operating
  system you are using');?>.</li>
  <li>
  <?php echo _('Include details of what you are trying to do, and any error
  messages or other problems you experience');?>.</li>
</ol>
<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('To discuss Audacity with other users and developers, join our');?><a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a>.</p>
<?php
  include "../include/footer.inc.php";
?>
