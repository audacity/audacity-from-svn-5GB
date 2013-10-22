<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 Richard Ash
 * 2007 Vaughan Johnson
 * 2008-12 Gale Andrews
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

<h3><?=_('Audacity Forum for Questions or Problems')?></h3>
<p><?=_('<b>There is no technical support by e-mail or telephone.</b> Most questions are answered in our <a href="../help/faq">Frequently Asked Questions (FAQ)</a>, <a href="../help/documentation">documentation</a>, and the additional <a href="http://wiki.audacityteam.org/wiki/Category:Tutorial">Tutorials</a> and <a href="http://wiki.audacityteam.org/wiki/Category:Tips">Tips</a> on the <a href="http://wiki.audacityteam.org/wiki">Audacity Wiki</a>. All these resources are searchable using the search box top right of this page.')?></p>
<p>
<?php echo _('If you still need help, expert assistance is available on the');?>
  <a href="http://forum.audacityteam.org/">
<?php
  // i18n-hint: "Forum" is the link text to the Forum. Probably you should not
  // translate this link text unless there is a sub-forum in your language.
  echo _('Audacity Forum</a>.');?>
<?php
  echo _(' To post a question, <a href="http://forum.audacityteam.org/ucp.php?mode=register">register</a>, confirm your registration then log in. Choose the correct subforum (for example, "Windows" in the Audacity 2.X Forum), click "New Topic" top left, then submit your message.  Foreign language subforums are available for <a href="http://forum.audacityteam.org/viewforum.php?f=7">French</a>, <a href="http://audacity-forum.de/">German</a>, <a href="http://forum.audacityteam.org/viewforum.php?f=10">Russian</a> and <a href="http://forum.audacityteam.org/viewforum.php?f=8">Spanish</a>.');?>
</p>
<p><?=_('Always give plenty of information when asking questions on the Audacity Forum. You can find more help with joining and using the Audacity Forum <a href="http://wiki.audacityteam.org/wiki/Asking_Questions">here</a>.')?></p>

<h3 id="feedback"><?=_('Suggestions, Bug Reports, Feature Requests and Patches')?></h3>
<p>
<?php
   // i18n-hint: the encoding inside the <a href> tag between the two 
   // "echo" strings obscures the e-mail address from (at least some) 
   // harvesting bots. Please translate the strings above and below
   // the <a href> tag, and ignore the line with the <a href> tag itself.   
   echo _('You can ')?>
   <a href="&#109;&#97;&#x69;&#x6c;&#x74;&#111;&#58;&#102;&#101;&#101;&#100;&#x62;&#97;&#99;&#107;&#x40;&#97;&#x75;&#x64;&#97;&#x63;&#105;&#116;&#x79;&#116;&#101;&#97;&#x6d;&#46;&#111;&#114;&#x67;"> 
<?php
   echo _('e-mail us</a> about any Audacity matter, but <b>not for technical support</b> - this is provided on the <a href="http://forum.audacityteam.org/">Audacity Forum</a>. Please write in <b>English language only.</b> Please tell us your experiences of the Audacity program, documentation or web site. We particularly welcome the following:</p>

<p>
<ul>
<li>Reports of possible program bugs</li>
<li>Requests for new or enhanced features</li>
<li>Offers to <a href="http://wiki.audacityteam.org/wiki/Contribute">contribute</a> to the Audacity project.');?></li>
</ul>
</p>

<p><?=_('<b>Bug reports:</b> Please check both the <a href="http://wiki.audacityteam.org/wiki/Release_Notes">Release Notes</a> for your specific version of Audacity 2.0 and <a href="http://wiki.audacityteam.org/wiki/Reporting_Bugs">Reporting Bugs</a> <b>before</b> making a report. We are not fixing bugs in the legacy 1.2 or 1.3 series - please upgrade to the <a href="/download/">latest 2.0 version</a> of Audacity instead.');?></p>

<div class="advice">
<p><?=_('<b>Please include the following information.</b> This is important.</p>
<ol>
  <li><b>The exact three-section version number of Audacity</b> (for example, 2.0.0) - you can check this at Help > About Audacity, or Audacity > About Audacity on Mac</li>
  <li><b>Your operating system</b> (for example, Windows 7 Service Pack 1 or Intel Mac OS X 10.4)</li>
  <li><b>Details of what you were trying to do</b>, including what steps led to the problem occurring, and the text of any error messages.</li>
</ol>
</div>

<p><b>Patches:</b> Code patches to fix problems or enhance features will be gratefully received. Please follow the guidance on the <a href="http://wiki.audacityteam.org/wiki/SubmittingPatches">Submitting Patches</a> page on the <a href="http://wiki.audacityteam.org/">Audacity Wiki</a>.</p>')?>    

<h3><?php echo _('Discussion Lists')?></h3>
<p><?php echo _('Subscribe to our');?>
 <a href="../contact/lists"><?php
//i18n-hint: this is the link text to the mailing list page
echo _('mailing lists');?></a> <?=_('to discuss Audacity with our community of users and developers. C++ developers: join ')?> <a href="https://lists.sourceforge.net/lists/listinfo/audacity-devel"> <?=_('our developers\' list')?></a> <?=_('to learn about the Audacity source code and contribute to making Audacity even better!')?></p>
<?php
  include "../include/footer.inc.php";
?>