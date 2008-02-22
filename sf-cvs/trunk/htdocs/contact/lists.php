<?php
/*
 * Copyright 2005 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "lists";
  $pageTitle = _("Mailing Lists");
  include "../include/header.inc.php";

  $subscriberOnlyStr = _("You must subscribe to send messages to this list.");
	// i18n-hint: The "%s" will be replaced by the name of a mailing list.
  $archiveStr = _("%s archives and subscription information");

	echo "<h2>$pageTitle</h2>";

	// i18n-hint: Please add a note that these lists are in English.  If there are
	// Audacity mailing lists or forums in your language, please link to them here.
	echo "<p>"._('These mailing lists are for feedback or discussion about the Audacity audio editor.  </p>
<p><b>Note:</b> In line with our <a href="../contact/privacy">Privacy Policy</a>, these are public mailing lists - messages are seen by all list subscribers, and posted on several web sites where the messages are 
archived.</p>');
?>

<dl>
<dt id="feedback">audacity-feedback</dt>
  <dd>
    <p><?=_('To give us feedback that does not require a personal response, send an email to the <a
href="&#x6d;&#x61;&#x69;&#x6c;&#x74;&#111;&#x3a;&#x61;&#117;&#x64;&#97;&#x63;&#105;&#x74;&#x79;&#x2d;&#x66;&#x65;&#101;&#100;&#x62;&#x61;&#99;&#x6b;&#x40;&#108;&#x69;&#x73;&#116;&#115;&#x2e;&#x73;&#x6f;&#x75;&#114;&#99;&#x65;&#x66;&#111;&#x72;&#x67;&#x65;&#x2e;&#110;&#x65;&#x74;">audacity-feedback</a>
list. Tell us your experiences of the Audacity program, documentation or web site, report possible program bugs or suggest new Audacity features. You do not need to subscribe to this list to send a message.')?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-feedback"><?php printf($archiveStr,
"audacity-feedback")?></a>
  </dd>

  <dt id="users">audacity-users</dt>
  <dd>
    <p><?=_('Discuss Audacity with other <a href="../community/users">users</a> and developers.')?>  
<?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-users"><?php printf($archiveStr, "audacity-users")?></a>
  </dd>

<dt id="devel">audacity-devel</dt>
  <dd>
    <p><?=_('For  <a href="../community/developers">developers</a> working with the Audacity source code and 
documentation, and others interested in following our development process or learning about compiling or developing 
Audacity source code.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel"><?php printf($archiveStr, "audacity-devel")?></a>
    <p><?=_('Developers may also subscribe to receive automated notification of <a href="http://lists.sourceforge.net/lists/listinfo/audacity-cvs">latest source code changes</a> and of <a 
href="http://lists.sourceforge.net/lists/listinfo/audacity-bugs">bugs</a> reported to the <a href="http://limpet.net/audacity/bugzilla/">Bugzilla</a> database.')?></p> 
  </dd>

  <dt id="translation">audacity-translation</dt>
  <dd>
    <p><?=_('For <a href="../community/translation">translators</a> localizing the Audacity software, web site and 
documentation.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-translation"><?php printf($archiveStr, "audacity-translation")?></a>
  </dd>

    <dt id="nyquist">audacity-nyquist</dt>
  <dd>
    <p><?=_('Share questions and tips about programming <a href="../help/nyquist">Nyquist plug-ins</a>.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-nyquist"><?php printf($archiveStr, "audacity-nyquist")?></a>
  </dd>


  <?php
  include "../include/footer.inc.php";
?>
