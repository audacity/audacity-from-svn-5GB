<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 Richard Ash
 * Copyright 2007 Gale Andrews, Vaughan Johnson
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

<h3><?=_('General Questions on How to Use Audacity')?></h3>
<p><?=_('First, check the <a href="../help/faq">Frequently Asked Questions (FAQ)</a>.')?></p>
<p>
  <?php echo _('If you still have a general question about how to use Audacity, please use the ');?>
  <a href="http://audacityteam.org/forum/"><?php 
  // i18n-hint: This is the link text to the Forum.
  echo _('Audacity Forum');?>
  </a><?php 
  // i18n-hint: These are instructions on how to use the forum. The name of the
  // link probably shouldn't be translated unless the forum interface supports
  // your language - currently German I think
  echo _('. There are foreign language Subforums for French, German, Russian, and Spanish. You can search the Forum and may well be able to find an immediate answer. If not, you can register and then post your question to other users. Log in and use the "Create New Thread" link.');?>
</p>
<p><?=_('You might also find helpful information by searching the <a href="http://sourceforge.net/mailarchive/forum.php?forum_name=audacity-help">Help list archive</a>, also available at <a href="http://www.nabble.com/audacity-help-f4506.html">Nabble</a> with an easier search interface.')?></p>


<h3><?=_('Bugs and Major Usage Problems')?></h3>
<p><?=_('Before you contact us, check the <a href="../help/faq">Frequently Asked Questions (FAQ)</a>.')?></p>
<p><?=_('If you still need help, email us at the <a href="mailto:audacity-help@lists.sourceforge.net">Help list, audacity-help@lists.sourceforge.net</a>. This is a public mailing list. For details, see our <a href="../contact/privacy">privacy policy</a>.')?></p>
<p><?=_('When you report a bug or problem, please:')?></p>
<ol>
  <li><?=_('Tell us what version of Audacity you are using (for example, 1.2.6).')?></li>
  <li><?=_('Tell us what operating system you are using (for example, Windows XP Service Pack 2 or Mac OS X 10.4).')?></li>
  <li><?=_('Include details of what you are trying to do, and any error messages or other problems you experience.')?></li>
  <li><?=_('Please ask your question in English if possible. Questions sent in languages other than English and French tend to get answered more slowly, because the Audacity Team members who handle the majority of the questions are English and French speakers.')?></li>
</ol>


<h3><?=_('Suggestions and Comments for Audacity Developers')?></h3>
<p>
  <?=_('Please email suggestions and comments for Audacity Developers to the <a href="mailto:audacity-help@lists.sourceforge.net">Help list, audacity-help@lists.sourceforge.net</a>. This is a public mailing list. For details, see our <a href="../contact/privacy">privacy policy</a>.')?>
</p>


<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('To discuss Audacity with other users and developers, join our');?>
 <a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a>.</p>
<?php
  include "../include/footer.inc.php";
?>
